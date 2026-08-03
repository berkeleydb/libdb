---
title: "Appendix D. DB_CONFIG Parameter Reference"
api-name: "Appendix D. DB_CONFIG Parameter Reference"
source: docs/api_reference/C/configuration_reference.html
---
## Appendix D. DB_CONFIG Parameter Reference

The following `DB_CONFIG` parameters can be used to manage various aspects of your application's database environment.

## DB_CONFIG Parameters

| DB_CONFIG Parameters | Description |
|----|----|
| <a href="add_data_dir_parameter.md" class="xref" title="add_data_dir">add_data_dir</a> | Sets the mutex alignment. |
| <a href="mutex_set_align_parameter.md" class="xref" title="mutex_set_align">mutex_set_align</a> | Sets the mutex alignment. |
| <a href="mutex_set_increment_parameter.md" class="xref" title="mutex_set_increment">mutex_set_increment</a> | Configures the number of additional mutexes to allocate. |
| <a href="mutex_set_max_parameter.md" class="xref" title="mutex_set_max">mutex_set_max</a> | Configures the total number of mutexes to allocate. |
| <a href="mutex_set_tas_spins_parameter.md" class="xref" title="mutex_set_tas_spins">mutex_set_tas_spins</a> | Specifies the number of times the test-and-set mutexes should spin without blocking. |
| <a href="rep_set_clockskew_parameter.md" class="xref" title="rep_set_clockskew">rep_set_clockskew</a> | Sets the clock skew ratio. |
| <a href="rep_set_config_parameter.md" class="xref" title="rep_set_config">rep_set_config</a> | Configures the Berkeley DB replication subsystem. |
| <a href="rep_set_limit_parameter.md" class="xref" title="rep_set_limit">rep_set_limit</a> | Sets record transmission throttling. |
| <a href="rep_set_nsites_parameter.md" class="xref" title="rep_set_nsites">rep_set_nsites</a> | Specifies the total number of sites in a replication group. |
| <a href="rep_set_priority_parameter.md" class="xref" title="rep_set_priority">rep_set_priority</a> | Specifies the database environment's priority. |
| <a href="rep_set_request_parameter.md" class="xref" title="rep_set_request">rep_set_request</a> | Sets a threshold before requesting retransmission of a missing message. |
| <a href="rep_set_timeout_parameter.md" class="xref" title="rep_set_timeout">rep_set_timeout</a> | Specifies a variety of replication timeout values. |
| <a href="repmgr_set_ack_policy_parameter.md" class="xref" title="repmgr_set_ack_policy">repmgr_set_ack_policy</a> | Specifies how master and client sites will handle acknowledgment. |
| <a href="repmgr_site_parameter.md" class="xref" title="repmgr_site">repmgr_site</a> | Identifies a Replication Manager host. |
| <a href="set_cachesize_parameter.md" class="xref" title="set_cachesize">set_cachesize</a> | Sets the size of the shared memory buffer pool. |
| <a href="set_cache_max_parameter.md" class="xref" title="set_cache_max">set_cache_max</a> | Sets the maximum size for set_cachesize parameter. |
| <a href="set_create_dir_parameter.md" class="xref" title="set_create_dir">set_create_dir</a> | Sets the directory path to create the access method database files. |
| <a href="set_data_len_parameter.md" class="xref" title="set_data_len">set_data_len</a> | Sets the maximum number of bytes displayed by some utilities. |
| <a href="set_flags_parameter.md" class="xref" title="set_flags">set_flags</a> | Configures a database environment. |
| <a href="set_intermediate_dir_mode_parameter.md" class="xref" title="set_intermediate_dir_mode">set_intermediate_dir_mode</a> | Configures the directory permissions. |
| <a href="set_lg_bsize_parameter.md" class="xref" title="set_lg_bsize">set_lg_bsize</a> | Sets the size of the in-memory log buffer. |
| <a href="set_lg_dir_parameter.md" class="xref" title="set_lg_dir">set_lg_dir</a> | Sets the path of the directory for logging files. |
| <a href="set_lg_filemode_parameter.md" class="xref" title="set_lg_filemode">set_lg_filemode</a> | Sets the absolute file mode for created log files. |
| <a href="set_lg_max_parameter.md" class="xref" title="set_lg_max">set_lg_max</a> | Sets the maximum size of a single file in the log. |
| <a href="set_lg_regionmax_parameter.md" class="xref" title="set_lg_regionmax">set_lg_regionmax</a> | Sets the size of the underlying logging area. |
| <a href="set_lk_detect_parameter.md" class="xref" title="set_lk_detect">set_lk_detect</a> | Sets the maximum number of locking entities. |
| <a href="set_lk_max_lockers_parameter.md" class="xref" title="set_lk_max_lockers">set_lk_max_lockers</a> | Sets the maximum number of locking entities. |
| <a href="set_lk_max_locks_parameter.md" class="xref" title="set_lk_max_locks">set_lk_max_locks</a> | Sets the maximum number of locks supported by the Berkeley DB environment. |
| <a href="set_lk_max_objects_parameter.md" class="xref" title="set_lk_max_objects">set_lk_max_objects</a> | Sets the maximum number of locked objects. |
| <a href="set_lk_partitions_parameter.md" class="xref" title="set_lk_partitions">set_lk_partitions</a> | Sets the number of lock table partitions in the Berkeley DB environment. |
| <a href="log_set_config_parameter.md" class="xref" title="log_set_config">log_set_config</a> | Configures the Berkeley DB logging subsystem. |
| <a href="set_mp_max_openfd_parameter.md" class="xref" title="set_mp_max_openfd">set_mp_max_openfd</a> | Limits the number of file descriptors the library will open concurrently when flushing dirty pages from the cache. |
| <a href="set_mp_max_write_parameter.md" class="xref" title="set_mp_max_write">set_mp_max_write</a> | Limits the number of sequential write operations |
| <a href="set_mp_mmapsize_parameter.md" class="xref" title="set_mp_mmapsize">set_mp_mmapsize</a> | Sets the maximum file size. |
| <a href="set_open_flags_parameter.md" class="xref" title="set_open_flags">set_open_flags</a> | Initializes specific subsystems of the Berkeley DB environment. |
| <a href="set_shm_key_parameter.md" class="xref" title="set_shm_key">set_shm_key</a> | Configures the database environment's base segment ID. |
| <a href="set_thread_count_parameter.md" class="xref" title="set_thread_count">set_thread_count</a> | Declares an approximate number of threads in the database environment. |
| <a href="set_timeout_parameter.md" class="xref" title="set_timeout">set_timeout</a> | Sets timeout values for locks or transactions. |
| <a href="set_tmp_dir_parameter.md" class="xref" title="set_tmp_dir">set_tmp_dir</a> | Specifies the directory path of temporary files. |
| <a href="set_tx_max_parameter.md" class="xref" title="set_tx_max">set_tx_max</a> | Configures support of simultaneously active transactions. |
| <a href="set_verbose_parameter.md" class="xref" title="set_verbose">set_verbose</a> | Enables/disables the Berkeley DB message output. |
