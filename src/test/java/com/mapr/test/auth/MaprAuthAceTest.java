package com.mapr.test.auth;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

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
			Map<String, ExpressionHolder> groups = new TreeMap<String, ExpressionHolder>();
			for (AceExpression ae : ace.getExpressions()) {
				if (groups.containsKey(ae.getGroupName())) {
					groups.get(ae.getGroupName()).getAceExpressions().add(ae);
				} else {
					List<AceExpression> groupList = new ArrayList<AceExpression>();
					groupList.add(ae);
					ExpressionHolder holder = new ExpressionHolder(groupList, ae.getGroupOperator());
					groups.put(ae.getGroupName(), holder);
				}
			}

			for (Entry<String, ExpressionHolder> expressions : groups.entrySet()) {
				buffer.append("(");
				String groupOperator = null;
				for (AceExpression expression : expressions.getValue().getAceExpressions()) {

					groupOperator = expressions.getValue().getGroupOperator();
					String operation = expression.getOperation();
					if (operation != null && !operation.equals(AceOperator.NOT.get())) {
						buffer.append(String.format("%s:%s %s", expression.getType(), expression.getValue(),
								expression.getOperation()));
					} else {
						buffer.append(String.format(" %s%s:%s", (operation != null ? operation : ""),
								expression.getType(), expression.getValue()));
					}

				}
				buffer.append(") ").append(groupOperator != null ? groupOperator + " " : "");
			}

			System.out.println(buffer.toString());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	class ExpressionHolder {
		private List<AceExpression> aceExpressions;
		private String groupOperator;

		ExpressionHolder(final List<AceExpression> aceExpressions, final String groupOperator) {
			this.aceExpressions = aceExpressions;
			this.groupOperator = groupOperator;
		}

		public List<AceExpression> getAceExpressions() {
			return aceExpressions;
		}

		public void setAceExpressions(List<AceExpression> aceExpressions) {
			this.aceExpressions = aceExpressions;
		}

		public String getGroupOperator() {
			return groupOperator;
		}

		public void setGroupOperator(String groupOperator) {
			this.groupOperator = groupOperator;
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
