/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2009, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 */
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.NetworkInformation;
using System.Text;
using System.Threading;
using System.Xml;
using NUnit.Framework;
using BerkeleyDB;

namespace CsharpAPITest
{
	[TestFixture]
	public class ReplicationTest : CSharpTestFixture
	{
		private EventWaitHandle clientStartSignal;
		private EventWaitHandle masterCloseSignal;

		private EventWaitHandle client1StartSignal;
		private EventWaitHandle client1ReadySignal;
		private EventWaitHandle client2StartSignal;
		private EventWaitHandle client2ReadySignal;
		private EventWaitHandle client3StartSignal;
		private EventWaitHandle client3ReadySignal;
		private EventWaitHandle masterLeaveSignal;

		List<uint> ports = new List<uint>();

		[TestFixtureSetUp]
		public void SetUp()
		{
			testFixtureName = "ReplicationTest";
			base.SetUpTestfixture();
		}

		[Test]
		public void TestRepMgrSite() 
		{
			testName = "TestRepMgrSite";
			SetUpTest(true);

			string masterHome = testHome + "\\Master";
			Configuration.ClearDir(masterHome);

			string clientHome = testHome + "\\Client";
			Configuration.ClearDir(clientHome);

			ports.Clear();
			AvailablePorts portGen = new AvailablePorts();
			uint mPort = portGen.Current;
			portGen.MoveNext();
			uint cPort = portGen.Current;

			// Open environment with replication configuration.
			DatabaseEnvironmentConfig cfg =
			    new DatabaseEnvironmentConfig();
			cfg.Create = true;
			cfg.RunRecovery = true;
			cfg.UseLocking = true;
			cfg.UseLogging = true;
			cfg.UseMPool = true;
			cfg.UseReplication = true;
			cfg.FreeThreaded = true;
			cfg.UseTxns = true;
			cfg.EventNotify = new EventNotifyDelegate(stuffHappened);

			cfg.RepSystemCfg = new ReplicationConfig();
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Port = mPort;
			cfg.RepSystemCfg.RepmgrSitesConfig[0].LocalSite = true;
			cfg.RepSystemCfg.RepmgrSitesConfig[0].GroupCreator = true;
			cfg.RepSystemCfg.Priority = 100;

			DatabaseEnvironment mEnv = DatabaseEnvironment.Open(
			    masterHome, cfg);
			mEnv.DeadlockResolution = DeadlockPolicy.DEFAULT;
			mEnv.RepMgrStartMaster(2);

			cfg.RepSystemCfg.RepmgrSitesConfig[0].Port = cPort;
			cfg.RepSystemCfg.RepmgrSitesConfig[0].GroupCreator = false;
			cfg.RepSystemCfg.Priority = 10;
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Port = mPort;
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Helper = true;
			DatabaseEnvironment cEnv = DatabaseEnvironment.Open(
			    clientHome, cfg);
			cEnv.RepMgrStartClient(2, false);

			/* Wait for client to start up */
			Thread.Sleep(2000);

			/*
			 * Verify the client info could be achived by master's
			 * remote site.
			 */ 			 
			Assert.AreEqual(1, mEnv.RepMgrRemoteSites.Length);
			RepMgrSite rsite = mEnv.RepMgrRemoteSites[0];
			Assert.AreEqual("127.0.0.1", rsite.Address.Host);
			Assert.AreEqual(cPort, rsite.Address.Port);
			Assert.AreEqual(1, rsite.EId);
			Assert.AreEqual(true, rsite.isConnected);
			Assert.AreEqual(false, rsite.isPeer);

			DbSite site = mEnv.RepMgrSite("127.0.0.1", mPort);
			Assert.AreEqual("127.0.0.1", site.Address.Host);
			Assert.AreEqual(mPort, site.Address.Port);
			Assert.AreEqual(0, site.EId);
			Assert.AreEqual(true, site.GroupCreator);
			Assert.AreEqual(true, site.LocalSite);
			Assert.AreEqual(false, site.Helper);
			Assert.AreEqual(false, site.Legacy);
			Assert.AreEqual(false, site.Peer);
			site.Close();

			site = mEnv.RepMgrSite("127.0.0.1", cPort);
			Assert.AreEqual("127.0.0.1", site.Address.Host);
			Assert.AreEqual(cPort, site.Address.Port);
			Assert.AreEqual(1, site.EId);
			Assert.AreEqual(false, site.GroupCreator);
			Assert.AreEqual(false, site.LocalSite);
			Assert.AreEqual(false, site.Helper);
			Assert.AreEqual(false, site.Legacy);
			Assert.AreEqual(false, site.Peer);
			site.Remove();

			cEnv.Close();
			mEnv.Close();

			/*
			 * Update the repmgr site, and verify it is updated in
			 * unmanged memory.
			 */ 			 
			rsite.Address = new ReplicationHostAddress(
			    "192.168.1.1", 1000);
			rsite.EId = 1024;
			rsite.isConnected = false;
			rsite.isPeer = true;
			Assert.AreEqual("192.168.1.1", rsite.Address.Host);
			Assert.AreEqual(1000, rsite.Address.Port);
			Assert.AreEqual(1024, rsite.EId);
			Assert.AreEqual(false, rsite.isConnected);
			Assert.AreEqual(true, rsite.isPeer);
		}

		[Test]
		public void TestRepMgr()
		{
			testName = "TestRepMgr";
			SetUpTest(true);

			// Initialize ports for master and client.
			ports.Clear();
			AvailablePorts portGen = new AvailablePorts();
			ports.Insert(0, portGen.Current);
			portGen.MoveNext();
			ports.Insert(1, portGen.Current);

			clientStartSignal = new AutoResetEvent(false);
			masterCloseSignal = new AutoResetEvent(false);

			Thread thread1 = new Thread(new ThreadStart(Master));
			Thread thread2 = new Thread(new ThreadStart(Client));

			// Start master thread before client thread.
			thread1.Start();
			Thread.Sleep(1000);
			thread2.Start();
			thread2.Join();
			thread1.Join();

			clientStartSignal.Close();
			masterCloseSignal.Close();
		}

		public void Master()
		{
			string home = testHome + "/Master";
			string dbName = "rep.db";
			Configuration.ClearDir(home);

			/*
			 * Configure and open environment with replication 
			 * application.
			 */
			DatabaseEnvironmentConfig cfg =
			    new DatabaseEnvironmentConfig();
			cfg.UseReplication = true;
			cfg.MPoolSystemCfg = new MPoolConfig();
			cfg.MPoolSystemCfg.CacheSize =
			    new CacheInfo(0, 20485760, 1);
			cfg.UseLocking = true;
			cfg.UseTxns = true;
			cfg.UseMPool = true;
			cfg.Create = true;
			cfg.UseLogging = true;
			cfg.RunRecovery = true;
			cfg.TxnNoSync = true;
			cfg.FreeThreaded = true;
			cfg.RepSystemCfg = new ReplicationConfig();
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Port = ports[0];
			cfg.RepSystemCfg.RepmgrSitesConfig[0].LocalSite = true;
			cfg.RepSystemCfg.Priority = 100;
			cfg.RepSystemCfg.BulkTransfer = true;
			cfg.RepSystemCfg.AckTimeout = 2000;
			cfg.RepSystemCfg.BulkTransfer = true;
			cfg.RepSystemCfg.CheckpointDelay = 1500;
			cfg.RepSystemCfg.Clockskew(102, 100);
			cfg.RepSystemCfg.ConnectionRetry = 10;
			cfg.RepSystemCfg.DelayClientSync = false;
			cfg.RepSystemCfg.ElectionRetry = 5;
			cfg.RepSystemCfg.ElectionTimeout = 3000;
			cfg.RepSystemCfg.FullElectionTimeout = 5000;
			cfg.RepSystemCfg.HeartbeatMonitor = 100;
			cfg.RepSystemCfg.HeartbeatSend = 10;
			cfg.RepSystemCfg.LeaseTimeout = 1300;
			cfg.RepSystemCfg.AutoInit = true;
			cfg.RepSystemCfg.NoBlocking = false;
			cfg.RepSystemCfg.RepMgrAckPolicy = 
			    AckPolicy.ALL_PEERS;
			cfg.RepSystemCfg.RetransmissionRequest(10, 100);
			cfg.RepSystemCfg.Strict2Site = true;
			cfg.RepSystemCfg.UseMasterLeases = false;
			cfg.EventNotify = new EventNotifyDelegate(stuffHappened);
			DatabaseEnvironment env = DatabaseEnvironment.Open(
			    home, cfg);

			// Get initial replication stats.
			ReplicationStats repStats = env.ReplicationSystemStats();
			env.PrintReplicationSystemStats();
			Assert.AreEqual(100, repStats.EnvPriority);
			Assert.AreEqual(1, 
			    repStats.CurrentElectionGenerationNumber);
			Assert.AreEqual(0, repStats.CurrentGenerationNumber);
			Assert.AreEqual(0, repStats.AppliedTransactions);
			Assert.AreEqual(0, repStats.ElectionDataGeneration);

			// Start a master site with replication manager.
			env.RepMgrStartMaster(3);

			// Open a btree database and write some data.
			Transaction txn = env.BeginTransaction();
			BTreeDatabaseConfig dbConfig = 
			    new BTreeDatabaseConfig();
			dbConfig.Creation = CreatePolicy.IF_NEEDED;
			dbConfig.Env = env;
			dbConfig.PageSize = 512;
			BTreeDatabase db = BTreeDatabase.Open(dbName, 
			    dbConfig, txn);
			txn.Commit();
			txn = env.BeginTransaction();
			for (int i = 0; i < 5; i++)
				db.Put(new DatabaseEntry(BitConverter.GetBytes(i)),
				    new DatabaseEntry(BitConverter.GetBytes(i)), txn);
			txn.Commit();

			Console.WriteLine(
			    "Master: Finished initialization and data#1.");

			// Client site could enter now.
			clientStartSignal.Set();
			Console.WriteLine(
			    "Master: Wait for Client to join and get #1.");

			Console.WriteLine("...");

			// Put some new data into master site.
			txn = env.BeginTransaction();
			for (int i = 10; i < 15; i++)
				db.Put(new DatabaseEntry(BitConverter.GetBytes(i)), 
				    new DatabaseEntry(BitConverter.GetBytes(i)),
				    txn);
			txn.Commit();
			Console.WriteLine(
			    "Master: Write something new, data #2.");
			Console.WriteLine("Master: Wait for client to read #2...");

			// Get the stats.
			repStats = env.ReplicationSystemStats(true);
			env.PrintReplicationSystemStats();
			Assert.LessOrEqual(0, repStats.AppliedTransactions);
			Assert.LessOrEqual(0, repStats.AwaitedLSN.LogFileNumber);
			Assert.LessOrEqual(0, repStats.AwaitedLSN.Offset);
			Assert.LessOrEqual(0, repStats.AwaitedPage);
			Assert.LessOrEqual(0, repStats.BadGenerationMessages);
			Assert.LessOrEqual(0, repStats.BulkBufferFills);
			Assert.LessOrEqual(0, repStats.BulkBufferOverflows);
			Assert.LessOrEqual(0, repStats.BulkBufferTransfers);
			Assert.LessOrEqual(0, repStats.BulkRecordsStored);
			Assert.LessOrEqual(0, repStats.ClientServiceRequests);
			Assert.LessOrEqual(0, repStats.ClientServiceRequestsMissing);
			Assert.IsInstanceOf(typeof(bool), repStats.ClientStartupComplete);
			Assert.AreEqual(2, repStats.CurrentElectionGenerationNumber);
			Assert.AreEqual(1, repStats.CurrentGenerationNumber);
			Assert.LessOrEqual(0, repStats.CurrentQueuedLogRecords);
			Assert.LessOrEqual(0, repStats.CurrentWinner);
			Assert.LessOrEqual(0, repStats.CurrentWinnerMaxLSN.LogFileNumber);
			Assert.LessOrEqual(0, repStats.CurrentWinnerMaxLSN.Offset);
			Assert.LessOrEqual(0, repStats.DuplicateLogRecords);
			Assert.LessOrEqual(0, repStats.DuplicatePages);
			Assert.LessOrEqual(0, repStats.DupMasters);
			Assert.LessOrEqual(0, repStats.ElectionGenerationNumber);
			Assert.LessOrEqual(0, repStats.ElectionPriority);
			Assert.LessOrEqual(0, repStats.Elections);
			Assert.LessOrEqual(0, repStats.ElectionStatus);
			Assert.LessOrEqual(0, repStats.ElectionsWon);
			Assert.LessOrEqual(0, repStats.ElectionTiebreaker);
			Assert.LessOrEqual(0, repStats.ElectionTimeSec);
			Assert.LessOrEqual(0, repStats.ElectionTimeUSec);
			Assert.AreEqual(repStats.EnvID, repStats.MasterEnvID);
			Assert.LessOrEqual(0, repStats.EnvPriority);
			Assert.LessOrEqual(0, repStats.FailedMessageSends);
			Assert.LessOrEqual(0, repStats.ForcedRerequests);
			Assert.LessOrEqual(0, repStats.IgnoredMessages);
			Assert.LessOrEqual(0, repStats.MasterChanges);
			Assert.LessOrEqual(0, repStats.MasterEnvID);
			Assert.LessOrEqual(0, repStats.MaxLeaseSec);
			Assert.LessOrEqual(0, repStats.MaxLeaseUSec);
			Assert.LessOrEqual(0, repStats.MaxPermanentLSN.Offset);
			Assert.LessOrEqual(0, repStats.MaxQueuedLogRecords);
			Assert.LessOrEqual(0, repStats.MessagesSent);
			Assert.LessOrEqual(0, repStats.MissedLogRecords);
			Assert.LessOrEqual(0, repStats.MissedPages);
			Assert.LessOrEqual(0, repStats.NewSiteMessages);
			Assert.LessOrEqual(repStats.MaxPermanentLSN.LogFileNumber, 
			    repStats.NextLSN.LogFileNumber);
			if (repStats.MaxPermanentLSN.LogFileNumber ==
			    repStats.NextLSN.LogFileNumber)
				Assert.Less(repStats.MaxPermanentLSN.Offset,
				    repStats.NextLSN.Offset);
			Assert.LessOrEqual(0, repStats.NextPage);
			Assert.LessOrEqual(0, repStats.Outdated);
			Assert.LessOrEqual(0, repStats.QueuedLogRecords);
			Assert.LessOrEqual(0, repStats.ReceivedLogRecords);
			Assert.LessOrEqual(0, repStats.ReceivedMessages);
			Assert.LessOrEqual(0, repStats.ReceivedPages);
			Assert.LessOrEqual(0, repStats.RegisteredSites);
			Assert.LessOrEqual(0, repStats.RegisteredSitesNeeded);
			Assert.LessOrEqual(0, repStats.Sites);
			Assert.LessOrEqual(0, repStats.StartSyncMessagesDelayed);
			Assert.AreEqual(2, repStats.Status);
			Assert.LessOrEqual(0, repStats.Throttled);
			Assert.LessOrEqual(0, repStats.Votes);	

			// Get replication manager statistics.
			RepMgrStats repMgrStats = env.RepMgrSystemStats(true);
			Assert.LessOrEqual(0, repMgrStats.DroppedConnections);
			Assert.LessOrEqual(0, repMgrStats.DroppedMessages);
			Assert.LessOrEqual(0, repMgrStats.FailedConnections);
			Assert.LessOrEqual(0, repMgrStats.FailedMessages);
			Assert.LessOrEqual(0, repMgrStats.QueuedMessages);

			// Print them out.
			env.PrintRepMgrSystemStats();

			// Wait until client has finished reading.
			masterCloseSignal.WaitOne();
			Console.WriteLine("Master: Leave as well.");

			// Close all.
			db.Close(false);
			env.LogFlush();
			env.Close();
		}

		public void Client()
		{
			string home = testHome + "/Client";
			Configuration.ClearDir(home);

			clientStartSignal.WaitOne();
			Console.WriteLine("Client: Join the replication");

			// Open a environment.
			DatabaseEnvironmentConfig cfg =
			    new DatabaseEnvironmentConfig();
			cfg.UseReplication = true;
			cfg.MPoolSystemCfg = new MPoolConfig();
			cfg.MPoolSystemCfg.CacheSize = 
			    new CacheInfo(0, 20485760, 1);
			cfg.UseLocking = true;
			cfg.UseTxns = true;
			cfg.UseMPool = true;
			cfg.Create = true;
			cfg.UseLogging = true;
			cfg.RunRecovery = true;
			cfg.TxnNoSync = true;
			cfg.FreeThreaded = true;
			cfg.LockTimeout = 50000;
			cfg.RepSystemCfg = new ReplicationConfig();
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Port = ports[1];
			cfg.RepSystemCfg.RepmgrSitesConfig[0].LocalSite = true;
			cfg.RepSystemCfg.Priority = 10;
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Port = ports[0];
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Helper = true;
			cfg.EventNotify = new EventNotifyDelegate(stuffHappened);
			DatabaseEnvironment env = DatabaseEnvironment.Open(
			    home, cfg);

			// Start a client site with replication manager.
			env.RepMgrStartClient(3, false);

			// Leave enough time to sync.
			Thread.Sleep(20000);

			// Open database.
			BTreeDatabaseConfig dbConfig = 
			    new BTreeDatabaseConfig();
			dbConfig.Creation = CreatePolicy.NEVER;
			dbConfig.AutoCommit = true;
			dbConfig.Env = env;
			dbConfig.PageSize = 512;
			BTreeDatabase db = BTreeDatabase.Open("rep.db", 
			    dbConfig);

			// Write data into database.
			Console.WriteLine("Client: Start reading data #1.");
			for (int i = 0; i < 5; i++)
				db.GetBoth(new DatabaseEntry(
				    BitConverter.GetBytes(i)), new DatabaseEntry(
				    BitConverter.GetBytes(i)));

			// Leave sometime for client to read new data from master.
			Thread.Sleep(20000);

			/* 
			 * Read the data. All data exists in master site should
			 * appear in the client site.
			 */ 
			Console.WriteLine("Client: Start reading data #2.");
			for (int i = 10; i < 15; i++)
				db.GetBoth(new DatabaseEntry(
				    BitConverter.GetBytes(i)), new DatabaseEntry(
				    BitConverter.GetBytes(i)));

			// Get the latest replication subsystem statistics.
			ReplicationStats repStats = env.ReplicationSystemStats();
			Assert.IsTrue(repStats.ClientStartupComplete);
			Assert.LessOrEqual(0, repStats.DuplicateLogRecords);
			Assert.LessOrEqual(0, repStats.EnvID);
			Assert.LessOrEqual(0, repStats.NextPage);
			Assert.LessOrEqual(0, repStats.ReceivedPages);
			Assert.AreEqual(1, repStats.Status);

			// Close all.
			db.Close(false);
			env.LogFlush();
			env.Close();
			Console.WriteLine(
			    "Client: All data is read. Leaving the replication");

			// The master is closed after client's close.
			masterCloseSignal.Set();
		}

		private void stuffHappened(NotificationEvent eventCode, byte[] info)
		{
			switch (eventCode)
			{
				case NotificationEvent.REP_CLIENT:
				Console.WriteLine("Event: CLIENT");
					break;
				case NotificationEvent.REP_CONNECT_BROKEN:
					Console.WriteLine("Event: REP_CONNECT_BROKEN");
					break;
				case NotificationEvent.REP_CONNECT_ESTD:
					Console.WriteLine("Event: REP_CONNECT_ESTD");
					break;
				case NotificationEvent.REP_CONNECT_TRY_FAILED:
					Console.WriteLine("Event: REP_CONNECT_TRY_FAILED");
					break;
				case NotificationEvent.REP_MASTER:
					Console.WriteLine("Event: MASTER");
					break;
				case NotificationEvent.REP_NEWMASTER:
					Console.WriteLine("Event: NEWMASTER");
					break;
				case NotificationEvent.REP_LOCAL_SITE_REMOVED:
					Console.WriteLine("Event: REP_LOCAL_SITE_REMOVED");
					break;
				case NotificationEvent.REP_SITE_ADDED:
					Console.WriteLine("Event: REP_SITE_ADDED");
					break;
				case NotificationEvent.REP_SITE_REMOVED:
					Console.WriteLine("Event: REP_SITE_REMOVED");
					break;
				case NotificationEvent.REP_STARTUPDONE:
					Console.WriteLine("Event: REP_STARTUPDONE");
					break;
				case NotificationEvent.REP_PERM_FAILED:
					Console.WriteLine("Event: Insufficient Acks.");
					break;
				default:
					Console.WriteLine("Event: {0}", eventCode);
					break;
			}
		}

		[Test]
		public void TestElection()
		{
			testName = "TestElection";
			SetUpTest(true);

			// Initialize ports for one master, and three clients.
			ports.Clear();
			AvailablePorts portGen = new AvailablePorts();
			ports.Insert(0, portGen.Current);
			portGen.MoveNext();
			ports.Insert(1, portGen.Current);
			portGen.MoveNext();
			ports.Insert(2, portGen.Current);
			portGen.MoveNext();
			ports.Insert(3, portGen.Current);

			client1StartSignal = new AutoResetEvent(false);
			client2StartSignal = new AutoResetEvent(false);
			client1ReadySignal = new AutoResetEvent(false);
			client2ReadySignal = new AutoResetEvent(false);
			client3StartSignal = new AutoResetEvent(false);
			client3ReadySignal = new AutoResetEvent(false);
			masterLeaveSignal = new AutoResetEvent(false);

			Thread thread1 = new Thread(
			    new ThreadStart(UnstableMaster));
			Thread thread2 = new Thread(
			    new ThreadStart(StableClient1));
			Thread thread3 = new Thread(
			    new ThreadStart(StableClient2));
			Thread thread4 = new Thread(
			    new ThreadStart(StableClient3));

			thread1.Start();
			Thread.Sleep(1000);
			thread2.Start();
			thread3.Start();
			thread4.Start();

			thread4.Join();
			thread3.Join();
			thread2.Join();
			thread1.Join();

			client1StartSignal.Close();
			client2StartSignal.Close();
			client1ReadySignal.Close();
			client2ReadySignal.Close();
			client3ReadySignal.Close();
			client3StartSignal.Close();
			masterLeaveSignal.Close();
		}

		public void UnstableMaster()
		{
			string home = testHome + "/UnstableMaster";
			Configuration.ClearDir(home);

			// Open environment with replication configuration.
			DatabaseEnvironmentConfig cfg =
			    new DatabaseEnvironmentConfig();
			cfg.UseReplication = true;
			cfg.MPoolSystemCfg = new MPoolConfig();
			cfg.MPoolSystemCfg.CacheSize = 
			    new CacheInfo(0, 20485760, 1);
			cfg.UseLocking = true;
			cfg.UseTxns = true;
			cfg.UseMPool = true;
			cfg.Create = true;
			cfg.UseLogging = true;
			cfg.RunRecovery = true;
			cfg.TxnNoSync = true;
			cfg.FreeThreaded = true;
			cfg.RepSystemCfg = new ReplicationConfig();
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Port = ports[0];
			cfg.RepSystemCfg.RepmgrSitesConfig[0].LocalSite = true;
			cfg.RepSystemCfg.Priority = 200;
			cfg.RepSystemCfg.ElectionRetry = 10;
			cfg.RepSystemCfg.RepMgrAckPolicy = AckPolicy.ALL;
			cfg.EventNotify = new EventNotifyDelegate(stuffHappened);
			DatabaseEnvironment env = DatabaseEnvironment.Open(
			    home, cfg);
			env.DeadlockResolution = DeadlockPolicy.DEFAULT;

			// Start as master site.
			env.RepMgrStartMaster(3);

			Console.WriteLine("Master: Finish initialization");

			// Notify clients to join.
			client1StartSignal.Set();
			client2StartSignal.Set();
			client3StartSignal.Set();

			// Wait for initialization of all clients.
			client1ReadySignal.WaitOne();
			client2ReadySignal.WaitOne();
			client3ReadySignal.WaitOne();

			foreach (RepMgrSite site in env.RepMgrRemoteSites)
			{
				Assert.AreEqual("127.0.0.1", site.Address.Host);
				Assert.IsTrue(ports.Contains(site.Address.Port));
			}

			// After all of them are ready, close the current master.
			Console.WriteLine("Master: Unexpected leave.");
			env.LogFlush();
			env.Close();
			masterLeaveSignal.Set();
		}

		public void StableClient1()
		{
			string home = testHome + "/StableClient1";
			Configuration.ClearDir(home);

			// Get notification from master and start the #1 client.
			client1StartSignal.WaitOne();
			Console.WriteLine("Client1: Join the replication");

			// Open the environment.
			DatabaseEnvironmentConfig cfg =
			    new DatabaseEnvironmentConfig();
			cfg.UseReplication = true;
			cfg.MPoolSystemCfg = new MPoolConfig();
			cfg.MPoolSystemCfg.CacheSize = 
			    new CacheInfo(0, 20485760, 1);
			cfg.UseLocking = true;
			cfg.UseTxns = true;
			cfg.UseMPool = true;
			cfg.Create = true;
			cfg.UseLogging = true;
			cfg.RunRecovery = true;
			cfg.TxnNoSync = true;
			cfg.FreeThreaded = true;
			cfg.LockTimeout = 50000;
			cfg.RepSystemCfg = new ReplicationConfig();
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Port = ports[1];
			cfg.RepSystemCfg.RepmgrSitesConfig[0].LocalSite = true;
			cfg.RepSystemCfg.Priority = 10;
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[1] = new DbSiteConfig();
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Port = ports[0];
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Helper = true;
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[2] = new DbSiteConfig();
			cfg.RepSystemCfg.RepmgrSitesConfig[2].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[2].Port = ports[3];
			cfg.RepSystemCfg.RepmgrSitesConfig[2].Peer = true;
			cfg.RepSystemCfg.ElectionRetry = 10;
			cfg.RepSystemCfg.RepMgrAckPolicy = AckPolicy.NONE;
			cfg.EventNotify = new EventNotifyDelegate(stuffHappened);
			DatabaseEnvironment env = DatabaseEnvironment.Open(
			    home, cfg);
			env.DeadlockResolution = DeadlockPolicy.DEFAULT;

			// Start the client who won't raise any election.
			env.RepMgrStartClient(3, false);

			// Leave enough time to sync.
			Thread.Sleep(20000);

			// Wait for master's leave signal.
			masterLeaveSignal.WaitOne();

			/*
 			 * Set the master's leave signal so that other clients 
			 * could be informed.
			 */
			masterLeaveSignal.Set();

			// Leave sometime for client to hold election.
			Thread.Sleep(10000);

			env.LogFlush();
			env.Close();
			Console.WriteLine("Client1: Leaving the replication");
		}

		public void StableClient2()
		{
			string home = testHome + "/StableClient2";
			Configuration.ClearDir(home);

			client2StartSignal.WaitOne();
			Console.WriteLine("Client2: Join the replication");

			DatabaseEnvironmentConfig cfg = 
			    new DatabaseEnvironmentConfig();
			cfg.UseReplication = true;
			cfg.MPoolSystemCfg = new MPoolConfig();
			cfg.MPoolSystemCfg.CacheSize = 
			    new CacheInfo(0, 20485760, 1);
			cfg.UseLocking = true;
			cfg.UseTxns = true;
			cfg.UseMPool = true;
			cfg.Create = true;
			cfg.UseLogging = true;
			cfg.RunRecovery = true;
			cfg.TxnNoSync = true;
			cfg.FreeThreaded = true;
			cfg.LockTimeout = 50000;
			cfg.RepSystemCfg = new ReplicationConfig();
			cfg.RepSystemCfg = new ReplicationConfig();
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Port = ports[2];
			cfg.RepSystemCfg.RepmgrSitesConfig[0].LocalSite = true;
			cfg.RepSystemCfg.Priority = 20;
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Port = ports[0];
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Helper = true;
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[2].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[2].Port = ports[1];
			cfg.RepSystemCfg.RepmgrSitesConfig[2].Peer = true;
			cfg.RepSystemCfg.ElectionRetry = 10;
			cfg.RepSystemCfg.RepMgrAckPolicy = 
			    AckPolicy.ONE_PEER;
			cfg.EventNotify = new EventNotifyDelegate(stuffHappened);
			DatabaseEnvironment env = DatabaseEnvironment.Open(
			    home, cfg);
			env.DeadlockResolution = DeadlockPolicy.DEFAULT;

			// Start the client who will raise election if no master.
			env.RepMgrStartClient(3, true);

			// Leave enough time to sync.
			Thread.Sleep(20000);

			// The current client site is fully initialized.
			client2ReadySignal.Set();

			// Wait for master's leave signal.
			masterLeaveSignal.WaitOne();

			/*
 			 * Set the master's leave signal so that other clients 
			 * could be informed.
			 */
			masterLeaveSignal.Set();

			// Leave sometime for client to hold election.
			Thread.Sleep(5000);

			env.LogFlush();
			env.Close();
			Console.WriteLine("Client2: Leaving the replication");
		}

		public void StableClient3()
		{
			string home = testHome + "/StableClient3";
			Configuration.ClearDir(home);

			client3StartSignal.WaitOne();
			Console.WriteLine("Client3: Join the replication");

			DatabaseEnvironmentConfig cfg =
			    new DatabaseEnvironmentConfig();
			cfg.UseReplication = true;
			cfg.MPoolSystemCfg = new MPoolConfig();
			cfg.MPoolSystemCfg.CacheSize = 
			    new CacheInfo(0, 20485760, 1);
			cfg.UseLocking = true;
			cfg.UseTxns = true;
			cfg.UseMPool = true;
			cfg.Create = true;
			cfg.UseLogging = true;
			cfg.RunRecovery = true;
			cfg.TxnNoSync = true;
			cfg.FreeThreaded = true;
			cfg.LockTimeout = 50000;
			cfg.RepSystemCfg = new ReplicationConfig();
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[0].Port = ports[3];
			cfg.RepSystemCfg.RepmgrSitesConfig[0].LocalSite = true;
			cfg.RepSystemCfg.Priority = 80;
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Port = ports[0];
			cfg.RepSystemCfg.RepmgrSitesConfig[1].Helper = true;
			cfg.RepSystemCfg.RepmgrSitesConfig.Add(new DbSiteConfig());
			cfg.RepSystemCfg.RepmgrSitesConfig[2].Host = "127.0.0.1";
			cfg.RepSystemCfg.RepmgrSitesConfig[2].Port = ports[1];
			cfg.RepSystemCfg.RepmgrSitesConfig[2].Peer = true;
			cfg.EventNotify = new EventNotifyDelegate(stuffHappened);
			cfg.RepSystemCfg.ElectionRetry = 10;
			cfg.RepSystemCfg.RepMgrAckPolicy = AckPolicy.QUORUM;
			DatabaseEnvironment env = DatabaseEnvironment.Open(
			    home, cfg);
			env.DeadlockResolution = DeadlockPolicy.DEFAULT;

			env.RepMgrStartClient(3, false);

			// Leave enough time to sync with master.
			Thread.Sleep(20000);

			// The current client site is fully initialized.
			client3ReadySignal.Set();

			// The current client site is fully initialized.
			client1ReadySignal.Set();

			foreach (RepMgrSite site in env.RepMgrRemoteSites) {
			if (site.Address.Port == ports[3])
				Assert.IsTrue(site.isPeer);
			else
				Assert.IsFalse(site.isPeer);
			}

			// Wait for master's leave signal.
			masterLeaveSignal.WaitOne();

			/*
			 * Set the master's leave signal so that other clients 
			 * could be informed.
			 */
			masterLeaveSignal.Set();
			
			/*
			 * Master will leave the replication after all clients' 
			 * initialization. Leave sometime for master to leave 
			 * and for clients elect.
			 */
			Thread.Sleep(5000);

			ReplicationStats repStats = env.ReplicationSystemStats();
			Assert.LessOrEqual(0, repStats.Elections);
			Assert.LessOrEqual(0, repStats.ElectionTiebreaker);
			Assert.LessOrEqual(0,
			    repStats.ElectionTimeSec + repStats.ElectionTimeUSec);
			Assert.LessOrEqual(0, repStats.MasterChanges);
			Assert.LessOrEqual(0, repStats.NewSiteMessages);
			Assert.LessOrEqual(0, repStats.ReceivedLogRecords);
			Assert.LessOrEqual(0, repStats.ReceivedMessages);
			Assert.LessOrEqual(0, repStats.ReceivedPages);
			Assert.GreaterOrEqual(4, repStats.RegisteredSitesNeeded);
			Assert.LessOrEqual(0, repStats.Sites);

			/*
			 * Client 3 will be the new master. The Elected master should wait 
			 * until all other clients leave.
			 */
			Thread.Sleep(10000);

			env.LogFlush();
			env.Close();
			Console.WriteLine("Client3: Leaving the replication");
		}

		[Test]
		public void TestAckPolicy()
		{
			testName = "TestAckPolicy";
			SetUpTest(true);

			SetRepMgrAckPolicy(testHome + "_ALL", AckPolicy.ALL);
			SetRepMgrAckPolicy(testHome + "_ALL_AVAILABLE",
			    AckPolicy.ALL_AVAILABLE);
			SetRepMgrAckPolicy(testHome + "_ALL_PEERS",
			    AckPolicy.ALL_PEERS);
			SetRepMgrAckPolicy(testHome + "_NONE", 
			    AckPolicy.NONE);
			SetRepMgrAckPolicy(testHome + "_ONE", 
			    AckPolicy.ONE);
			SetRepMgrAckPolicy(testHome + "_ONE_PEER",
			    AckPolicy.ONE_PEER);
			SetRepMgrAckPolicy(testHome + "_QUORUM", 
			    AckPolicy.QUORUM);
			SetRepMgrAckPolicy(testHome + "_NULL", null);
		}

		public void SetRepMgrAckPolicy(string home, AckPolicy policy)
		{
			Configuration.ClearDir(home);
			DatabaseEnvironmentConfig envConfig =
			    new DatabaseEnvironmentConfig();
			envConfig.Create = true;
			envConfig.UseLocking = true;
			envConfig.UseLogging = true;
			envConfig.UseMPool = true;
			envConfig.UseReplication = true;
			envConfig.UseTxns = true;
			DatabaseEnvironment env = DatabaseEnvironment.Open(
			    home, envConfig);
			if (policy != null)
			{
				env.RepMgrAckPolicy = policy;
				Assert.AreEqual(policy, env.RepMgrAckPolicy);
			}
			env.Close();
		}

		[Test]
		public void TestLocalSite() {
			testName = "TestLocalSite";
			SetUpTest(true);
			Configuration.ClearDir(testHome);
			DatabaseEnvironmentConfig envConfig =
			    new DatabaseEnvironmentConfig();
			envConfig.Create = true;
			envConfig.UseLocking = true;
			envConfig.UseLogging = true;
			envConfig.UseMPool = true;
			envConfig.UseReplication = true;
			envConfig.UseTxns = true;
			ReplicationHostAddress addr =
			    new ReplicationHostAddress("localhost:6060");
			ReplicationConfig repCfg = new ReplicationConfig();
			DbSiteConfig dbSiteConfig = new DbSiteConfig();
			dbSiteConfig.Host = addr.Host;
			dbSiteConfig.Port = addr.Port;
			dbSiteConfig.LocalSite = true;
			repCfg.RepmgrSitesConfig.Add(dbSiteConfig);
			envConfig.RepSystemCfg = repCfg;
			DatabaseEnvironment env =
			    DatabaseEnvironment.Open(testHome, envConfig);
			ReplicationHostAddress testAddr =
			    env.RepMgrLocalSite.Address;
			Assert.AreEqual(addr.Host, testAddr.Host);
			Assert.AreEqual(addr.Port, testAddr.Port);
			env.Close();
		}

		public class AvailablePorts : IEnumerable<uint>
		{
			private List<uint> availablePort = new List<uint>();
			private List<uint> usingPort = new List<uint>();
			private int position = -1;

			public AvailablePorts()
			{
				// Initial usingPort array with all TCP ports being used.
				IPGlobalProperties properties = IPGlobalProperties.GetIPGlobalProperties(); 
				foreach (IPEndPoint point in properties.GetActiveTcpListeners())
					usingPort.Add((uint)point.Port);

				// Initial availablePort array with available ports ranging from 10000 to 15000.
				for (uint i = 10000; i <= 15000; i++)
				{
					if (!usingPort.Contains(i))
						availablePort.Add(i);
				}
			}

			IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }

			public IEnumerator<uint> GetEnumerator()
			{
				while (MoveNext())
					yield return availablePort[position];
			}

			public bool MoveNext()
			{
				position++;
				return position < availablePort.Count;
			}

			public uint Current
			{
				get {
					if (position == -1)
						position = 0;
					return availablePort[position];
				}
			}
		}
	}
}
