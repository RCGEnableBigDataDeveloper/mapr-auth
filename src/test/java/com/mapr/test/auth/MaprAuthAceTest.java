package com.mapr.test.auth;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;
import java.util.List;

import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Application;
import javax.ws.rs.core.MediaType;

import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.moxy.xml.MoxyXmlFeature;
import org.glassfish.jersey.test.JerseyTest;
import org.glassfish.jersey.test.TestProperties;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.mapr.auth.ace.AceAccessType;
import com.mapr.auth.ace.AceExpression;
import com.mapr.auth.ace.AceOperator;
import com.mapr.auth.ace.MaprAce;
import com.mapr.auth.ace.MaprAceBuilder;
import com.mapr.auth.resource.MaprAceSupport;
import com.mapr.auth.server.MapRAuthServer;
import com.mapr.fs.MapRFileAce;

public class MaprAuthAceTest extends JerseyTest {

	@Override
	protected Application configure() {
		forceSet(TestProperties.CONTAINER_PORT, "8086");
		return MapRAuthServer.createApp();
	}

	@Override
	protected void configureClient(ClientConfig config) {
		config.register(new MoxyXmlFeature());
	}

	@Test
	public void testGetAce() {
		WebTarget webTarget = target().path("aces");
		MaprAce ace = webTarget.request(MediaType.APPLICATION_JSON).get(MaprAce.class);
		assertNotNull(ace);
	}

	@Test
	public void testPutAce() {
		WebTarget webTarget = target().path("aces");
		MaprAce ace = webTarget.request(MediaType.APPLICATION_JSON).get(MaprAce.class);
		webTarget.request(MediaType.APPLICATION_XML).put(Entity.entity(ace, MediaType.APPLICATION_XML));
		ace = webTarget.request(MediaType.APPLICATION_XML).get(MaprAce.class);
		assertEquals(ace, ace);
	}

	@Test
	public void testBuildExpression() {

		AceExpression a = new AceExpression("u", "user1", AceOperator.AND.get(), "group1", AceOperator.AND.get(), 0);
		AceExpression b = new AceExpression("g", "group1", AceOperator.NOT.get(), "group1", AceOperator.AND.get(), 1);
		AceExpression c = new AceExpression("u", "user2", AceOperator.AND.get(), "group2", null, 0);
		AceExpression d = new AceExpression("g", "group2", null, "group2", null, 1);

		List<AceExpression> expressions = Arrays.asList(a, b, c, d);

		MaprAce ace = new MaprAceBuilder().with($ -> {
			$.name = "my_ace";
			$.access = new AceAccessType("/tmp/mydir", MapRFileAce.AccessType.READDIR);
			$.expressions = expressions;
		}).build();

		MaprAceSupport support = new MaprAceSupport();
		String expression = support.buildExpression(ace);
		assertNotNull(expression);
	}
}
