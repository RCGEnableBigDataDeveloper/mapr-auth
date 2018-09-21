
package com.mapr.auth.resource;

import java.util.Arrays;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.mapr.auth.ace.AceExpression;
import com.mapr.auth.ace.AceOperator;
import com.mapr.auth.ace.AceAccessType;
import com.mapr.auth.ace.MaprAce;
import com.mapr.auth.ace.MaprAceBuilder;
import com.mapr.fs.MapRFileAce;

@Path("/aces")
public class MaprAceResource {

	private static MaprAce ace = createAce();

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public MaprAce getAce() {
		return ace;
	}

	@PUT
	@Consumes(MediaType.APPLICATION_XML)
	public void setAce(final MaprAce c) {
		setAce(c);
	}

	private static MaprAce createAce() {
		AceExpression a = new AceExpression("u", "user1", AceOperator.AND.get(), "group1", 0);
		AceExpression b = new AceExpression("g", "group1", AceOperator.NOT.get(), "group1", 1);

		List<AceExpression> expressions = Arrays.asList(a, b);

		MaprAce result = new MaprAceBuilder().with($ -> {
			$.name = "Mr.";
			$.access = new AceAccessType(System.currentTimeMillis(), MapRFileAce.AccessType.READDIR);
			$.expressions = expressions;
			$.operator = AceOperator.AND.get();
		}).build();

		return result;
	}
}
