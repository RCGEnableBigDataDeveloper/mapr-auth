package com.mapr.test.auth;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Application;
import javax.ws.rs.core.MediaType;

import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.moxy.xml.MoxyXmlFeature;
import org.glassfish.jersey.test.JerseyTest;
import org.glassfish.jersey.test.TestProperties;
import org.junit.Test;

import com.mapr.auth.ace.AceExpression;
import com.mapr.auth.ace.AceOperator;
import com.mapr.auth.ace.MaprAce;
import com.mapr.auth.server.MapRAuthServer;

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
		try {
			WebTarget webTarget = target().path("aces");
			MaprAce ace = webTarget.request(MediaType.APPLICATION_JSON).get(MaprAce.class);
			Appendable buffer = new StringBuilder();
			Map<String, List<AceExpression>> groups = new HashMap<String, List<AceExpression>>();
			boolean needsParenthisis = ace.getExpressions().size() > 1;
			if (needsParenthisis)
				buffer.append("(");
			for (AceExpression ae : ace.getExpressions()) {
				System.out.println();

				if (groups.containsKey(ae.getGroup())) {
					groups.get(ae.getGroup()).add(ae);
				} else {
					List<AceExpression> groupList = new ArrayList<AceExpression>();
					groupList.add(ae);
					groups.put(ae.getGroup(), groupList);
				}

				System.out.println(groups);
				
				if (ae.getOperation() != null && !ae.getOperation().equals(AceOperator.NOT.get())) {
					buffer.append(String.format("%s:%s %s", ae.getType(), ae.getValue(), ae.getOperation()));
				} else {
					buffer.append(String.format(" %s%s:%s", ae.getOperation(), ae.getType(), ae.getValue()));
				}
			}
			if (needsParenthisis)
				buffer.append("}");

			System.out.println(buffer.toString());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testPutAce() {
		WebTarget webTarget = target().path("aces");
		MaprAce ace = webTarget.request(MediaType.APPLICATION_JSON).get(MaprAce.class);
		webTarget.request(MediaType.APPLICATION_XML).put(Entity.entity(ace, MediaType.APPLICATION_XML));
		ace = webTarget.request(MediaType.APPLICATION_XML).get(MaprAce.class);
		assertEquals(ace, ace);
	}
}
