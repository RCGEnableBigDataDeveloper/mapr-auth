
package com.mapr.auth.resource;

import java.util.Arrays;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.apache.log4j.Logger;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Throwables;
import com.mapr.auth.ace.AceAccessType;
import com.mapr.auth.ace.AceExpression;
import com.mapr.auth.ace.AceOperator;
import com.mapr.auth.ace.MaprAce;
import com.mapr.auth.ace.MaprAceBuilder;
import com.mapr.fs.MapRFileAce;

@Path("/aces")
public final class MaprAceResource extends MaprAceSupport {

	private final Logger logger = Logger.getLogger(getClass());

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAce(@QueryParam("path") String path) {

		try {
			if (path.equals("mock"))
				return Response.ok(createMockAce()).build();
			
			List<MapRFileAce> aces = getMaprAce(path);
			return Response.ok(aces).build();

		} catch (Exception e) {
			logger.error("error occurred in getAce", e);
			logger.error(Throwables.getStackTraceAsString(e));
			e.printStackTrace();
		}
		return null;
	}

	@PUT
	@Consumes(MediaType.APPLICATION_XML)
	public Response setAce(final MaprAce c) {
		setMaprAce(c);
		return Response.ok(c).build();
	}

	private static MaprAce createMockAce() {

		AceExpression a = new AceExpression("u", "user1", AceOperator.AND.get(), "group1", AceOperator.AND.get(), 0);
		AceExpression b = new AceExpression("g", "group1", AceOperator.NOT.get(), "group1", AceOperator.AND.get(), 1);
		AceExpression c = new AceExpression("u", "user2", AceOperator.AND.get(), "group2", null, 0);
		AceExpression d = new AceExpression("g", "group2", null, "group2", null, 1);

		List<AceExpression> expressions = Arrays.asList(a, b, c, d);

		MaprAce result = new MaprAceBuilder().with($ -> {
			$.name = "my_ace";
			$.access = new AceAccessType("/tmp/mydir", MapRFileAce.AccessType.READDIR);
			$.expressions = expressions;
		}).build();

		ObjectMapper mapper = new ObjectMapper();
		try {
			System.err.println(mapper.writeValueAsString(result));
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		}

		return result;
	}
}
