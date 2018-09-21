package com.mapr.auth.server;

import java.io.IOException;
import java.net.URI;

import org.apache.log4j.Logger;
import org.glassfish.grizzly.http.server.CLStaticHttpHandler;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.moxy.xml.MoxyXmlFeature;
import org.glassfish.jersey.server.ResourceConfig;

import com.mapr.auth.context.MaprAuthContext;

public class MapRAuthServer {

	private static final Logger logger = Logger.getLogger(MapRAuthServer.class);

	private static final URI BASE_URI = URI.create(String.format("%s://%s:%s%s",
			MaprAuthContext.getProperty("mapr.auth.server.protocol"),
			MaprAuthContext.getProperty("mapr.auth.server.host"), MaprAuthContext.getProperty("mapr.auth.server.port"),
			MaprAuthContext.getProperty("mapr.auth.server.context.root")));

	public static void main(String[] args) {

		try {

			final HttpServer server = GrizzlyHttpServerFactory.createHttpServer(BASE_URI, createApp());
			Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
				@Override
				public void run() {
					server.shutdownNow();
				}
			}));
			server.start();

			CLStaticHttpHandler httpHandler = new CLStaticHttpHandler(HttpServer.class.getClassLoader(), "/static/");
			server.getServerConfiguration().addHttpHandler(httpHandler, "/");

			logger.info("vdm server started at " + BASE_URI);
			
			Thread.currentThread().join();

		} catch (IOException | InterruptedException ex) {
			logger.error("failed to start server at " + BASE_URI, ex);
		}
	}

	public static ResourceConfig createApp() {
		return new ResourceConfig().packages("com.mapr.auth.resource").register(new MoxyXmlFeature());
	}
}
