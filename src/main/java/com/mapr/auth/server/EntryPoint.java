package com.mapr.auth.server;

import java.io.IOException;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import java.sql.SQLException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.sql.DriverManager;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.uhg.mapr.hdfs.ConnectionConfig;
import com.uhg.mapr.hdfs.HdfsConnection;

@Path("/entry-point")
public class EntryPoint {

	private final static ObjectMapper mapper = new ObjectMapper();

	@GET
	@Path("hdfs")
	@Produces(MediaType.TEXT_PLAIN)
	public String hdfs() {

		System.out.println("TEST");
		ConnectionConfig c = new ConnectionConfig();
		HdfsConnection hdfs = new HdfsConnection(c);
		try {
			hdfs.start("/user/eperler");
			return mapper.writeValueAsString(hdfs.items);
//			for(Map.Entry<String, ItemDefinition> item: hdfs.items.entrySet()) {
//				System.out.println(item.getKey());
//			}

		} catch (IOException e) {
			e.printStackTrace();
		}

		return null;
	}

	@GET
	@Path("hive")
	@Produces(MediaType.TEXT_PLAIN)
	public String hive() {

		String driverName = "org.apache.hive.jdbc.HiveDriver";

		try {
			Class.forName(driverName);

			Connection con = DriverManager.getConnection("jdbc:hive2://dbslp1620:5181,dbslp1621:5181,dbslp1711:5181/;serviceDiscoveryMode=zooKeeper;zooKeeperNamespace=hiveserver2", "hive", "");
			Statement stmt = con.createStatement();
			String sql = ("show tables");
			ResultSet res = stmt.executeQuery(sql);
			if (res.next()) {
				System.out.println(res.getString(1));
			}
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}

		return null;
	}
}