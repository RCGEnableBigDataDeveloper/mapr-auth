package com.mapr.auth.context;

import java.io.InputStream;
import java.sql.Connection;
import java.sql.SQLException;

import org.apache.log4j.Logger;

import com.mchange.v2.c3p0.ComboPooledDataSource;

public class MaprAuthDataSource {

	private static ComboPooledDataSource cpds;

	static final Logger logger = Logger.getLogger(MaprAuthDataSource.class);

	static String DATALAKE_SECRET_KEY = "datalake.key";

	static {

		cpds = new ComboPooledDataSource();
		try {

			cpds.setDriverClass(MaprAuthContext.getProperty("mapr-auth.database.driver"));
			cpds.setJdbcUrl(MaprAuthContext.getProperty("mapr-auth.database.url"));
			cpds.setUser(MaprAuthContext.getProperty("mapr-auth.database.user"));

			String pwd = MaprAuthContext.getProperty("mapr-auth.database.password");
			String secretKey = System.getProperty(DATALAKE_SECRET_KEY);
			if (secretKey == null || "".equals(secretKey)) {
				logger.warn("unable to load secret key -Ddatalake.key arguement not set.");
				InputStream keyStream = ComboPooledDataSource.class.getResourceAsStream("/datalake.key");
				pwd = pwd != null ? MaprAuthContext.decrypt(keyStream, pwd) : "";
			} else {
				pwd = pwd != null ? MaprAuthContext.decrypt(secretKey, pwd) : "";
			}

			cpds.setPassword(pwd);
			cpds.setMinPoolSize(5);
			cpds.setAcquireIncrement(5);
			cpds.setMaxPoolSize(20);
			cpds.setPreferredTestQuery("select 1 as db_connection_test");
			cpds.setIdleConnectionTestPeriod(300);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static Connection getConnection() throws SQLException {
		stats();
		return cpds.getConnection();
	}

	protected static void stats() throws SQLException {
		logger.info("active: [" + cpds.getNumConnectionsDefaultUser() + "] " + " busy: ["
				+ cpds.getNumBusyConnectionsDefaultUser() + "] " + "idle: [" + cpds.getNumIdleConnectionsDefaultUser()
				+ "] ");
	}
}
