# test/soak/run_soak.ps1 -- Windows/MSVC resource-accounting soak.
#
# The Windows counterpart of test/soak/run.sh.  Builds test_soak_resources.c
# with cl.exe against the MSVC-built libdb static library, then runs the
# resource-accounting driver in a loop until DURATION_MIN minutes elapse.  Each
# iteration is a full pass over every workload, opening and closing a
# long-lived environment -- so the loop is the Windows form of the matrix's
# "hourly failchk + env close/reopen": the environment is torn down and rebuilt
# every pass, and any pass whose resources did not return to steady state fails
# the whole soak.
#
# A nonzero exit means a workload disagreed with its recorded expectation
# (a control leaked, or a known-leaker stayed flat).  Same contract as run.sh.
#
# Params:
#   -Build       path to the MSVC build output holding libdb.lib (and db.h via
#                build_windows\db.h on the include path)
#   -DurationMin minutes to loop (default 4320 = 72h; 0 => one pass, for smoke)
#   -N           transactions per workload per pass (default 2000)
#
# Run from a "x64 Native Tools Command Prompt" (so cl.exe/link.exe are on PATH)
# via:  pwsh -File test\soak\run_soak.ps1 -Build build_windows\x64\Release

param(
    [string]$Build = "build_windows\x64\Release",
    [int]$DurationMin = 4320,
    [int]$N = 2000
)
$ErrorActionPreference = "Stop"
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$root = Resolve-Path (Join-Path $here "..\..")
$src  = Join-Path $here "test_soak_resources.c"
$out  = Join-Path $here "build"
New-Item -ItemType Directory -Force -Path $out | Out-Null

# The MSVC libdb import library. The vs2010 solution's db project emits a DLL
# (libdb<ver>.dll) plus its import lib (libdb<ver>.lib) under the configuration
# output dir; link the import lib and put the DLL dir on PATH at run time.
$lib = $null
foreach ($cand in @("libdb.lib","libdb62.lib","libdb53.lib","libdb_static.lib")) {
    $p = Join-Path $root (Join-Path $Build $cand)
    if (Test-Path $p) { $lib = $p; break }
}
if (-not $lib) {
    # fall back: the libdb*.lib in the build dir (the import lib for the DLL)
    $lib = (Get-ChildItem (Join-Path $root $Build) -Filter libdb*.lib -ErrorAction SilentlyContinue |
            Select-Object -First 1).FullName
}
if (-not $lib) { Write-Error "no libdb .lib under $Build -- build the db project first"; exit 2 }
Write-Host "lib: $lib"
$dlldir = Split-Path -Parent $lib
$env:Path = "$dlldir;$env:Path"   # so the libdb DLL loads at run time

$winhdr = Join-Path $root "build_windows"   # Windows db.h / db_config.h live here
$bin = Join-Path $out "test_soak_resources.exe"
# /MD matches the DLL CRT the solution uses; link ws2_32/advapi32 as libdb does on Win.
& cl /nologo /O2 /MD /I "$winhdr" /I "$here" "$src" "$lib" `
    ws2_32.lib advapi32.lib /Fe:"$bin" /Fo:"$out\" 2>&1 | Write-Host
if ($LASTEXITCODE -ne 0) { Write-Error "cl.exe build failed"; exit 2 }
Write-Host "built $bin"

if ($DurationMin -eq 0) {
    & $bin -n $N
    exit $LASTEXITCODE
}

$deadline = (Get-Date).AddMinutes($DurationMin)
$pass = 0
Push-Location $out
try {
    while ((Get-Date) -lt $deadline) {
        $pass++
        $log = Join-Path $out ("soak-pass-{0:D5}.log" -f $pass)
        Write-Host ("=== pass {0} (until {1}) ===" -f $pass, $deadline)
        & $bin -n $N *>&1 | Tee-Object -FilePath $log
        $rc = $LASTEXITCODE
        if ($rc -ne 0) {
            Write-Error ("SOAK FAIL on pass {0} (exit {1}) -- see {2}" -f $pass, $rc, $log)
            exit $rc
        }
    }
} finally { Pop-Location }
Write-Host ("SOAK PASS: {0} passes over {1} minutes, every pass resource-steady" -f $pass, $DurationMin)
exit 0
