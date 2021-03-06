package com.mapr.auth.resource;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.apache.log4j.Logger;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Throwables;
import com.mapr.auth.ConnectionConfig;
import com.mapr.auth.ItemDefinition;
import com.mapr.auth.hdfs.HdfsConnection;
import com.mapr.auth.server.MapRAuthServer;
import com.mapr.auth.util.HttpUtil;

@Path("/cluster")
public class ClusterResource {

	private static final Logger logger = Logger.getLogger(MapRAuthServer.class);

	private final static ObjectMapper mapper = new ObjectMapper();

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response test(@QueryParam("test") String path) {

		try {
			return Response.ok(mapper.writeValueAsString("hello")).build();
		} catch (JsonProcessingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return null;

	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getHdfs(@QueryParam("hdfs") String path) {
		ConnectionConfig c = new ConnectionConfig();
		HdfsConnection hdfs = new HdfsConnection(c);
		try {
			hdfs.start("/user/eperler");

//		for(Map.Entry<String, ItemDefinition> item: hdfs.items.entrySet()) {
//			System.out.println(item.getKey());
//		}

			return Response.ok(mapper.writeValueAsString(hdfs.items)).build();

		} catch (IOException e) {
			e.printStackTrace();
		}

		return null;
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAce(@QueryParam("hive") String path) {

		try {

			Map<String, List<ItemDefinition>> map = new HashMap<String, List<ItemDefinition>>();

			Properties properties = new Properties();
			properties.setProperty("show.columns", "false");
			ConnectionConfig config = new ConnectionConfig();
			config.setClazz("com.rcggs.datalake.connect.hive.HiveConnection");
			config.setHost("127.0.0.1");
			config.setPort("10000");
			config.setPath("default");
			config.setProperties(properties);
			config.setType("hive2");
			String body = mapper.writeValueAsString(config);

			String res = HttpUtil.post("http://127.0.0.1:8080/sample/hello/hive/uuu", body);

			Map<String, List<ItemDefinition>> items = mapper.readValue(res,
					new TypeReference<Map<String, List<ItemDefinition>>>() {
					});
			List<ItemDefinition> children = new ArrayList<>();
			ItemDefinition idef = new ItemDefinition();
			Entry<String, List<ItemDefinition>> entry = items.entrySet().iterator().next();
			idef.setName(entry.getKey());

			map.put(idef.getName(), entry.getValue());

			System.out.println(mapper.writeValueAsString(map));

			return Response.ok(mapper.writeValueAsString(map)).build();

		} catch (Exception e) {
			logger.error("error occurred in hive connection", e);
			logger.error(Throwables.getStackTraceAsString(e));
			e.printStackTrace();
		}
		return null;
	}

	public static void main(String[] args) throws Exception {

		Map<String, List<ItemDefinition>> map = new HashMap<String, List<ItemDefinition>>();

		Properties properties = new Properties();
		properties.setProperty("show.columns", "false");
		ConnectionConfig config = new ConnectionConfig();
		config.setClazz("com.rcggs.datalake.connect.hive.HiveConnection");
		config.setHost("127.0.0.1");
		config.setPort("10000");
		config.setPath("default");
		config.setProperties(properties);
		config.setType("hive2");
		String body = mapper.writeValueAsString(config);

		String res = HttpUtil.post("http://127.0.0.1:8080/sample/hello/hive/uuu", body);

		Map<String, List<ItemDefinition>> items = mapper.readValue(res,
				new TypeReference<Map<String, List<ItemDefinition>>>() {
				});
		List<ItemDefinition> children = new ArrayList<>();
		ItemDefinition idef = new ItemDefinition();
		Entry<String, List<ItemDefinition>> entry = items.entrySet().iterator().next();
		idef.setName(entry.getKey());

		map.put(idef.getName(), entry.getValue());

		System.out.println(map);
	}
}
