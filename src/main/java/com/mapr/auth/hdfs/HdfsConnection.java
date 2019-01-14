package com.mapr.auth.hdfs;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.security.PrivilegedExceptionAction;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileStatus;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.security.UserGroupInformation;
import org.apache.log4j.Logger;

import com.mapr.auth.AbstractMapRConnection;
import com.mapr.auth.ConnectionConfig;
import com.mapr.auth.MapRConnection;
import com.mapr.auth.ItemDefinition;

public class HdfsConnection extends AbstractMapRConnection implements MapRConnection {

	private final Logger logger = Logger.getLogger(getClass());

	private FileSystem fileSystem;
	private ConnectionConfig config;
	private Configuration conf = null;

	public HdfsConnection(ConnectionConfig config) {
		super(config);
		this.config = config;

		try {
			conf = new Configuration();
			String uri = "maprfs:///";
			fileSystem = FileSystem.get(URI.create(uri), conf);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public Map<String, ItemDefinition> start(String baseUri) throws IOException {
		FileStatus[] status = fileSystem.listStatus(new Path(baseUri));
		return browse(baseUri, status, "/");
	}

	List<ItemDefinition> dirs = new ArrayList<ItemDefinition>();
	public Map<String, ItemDefinition> items = new LinkedHashMap<String, ItemDefinition>();

	private Map<String, ItemDefinition> browse(String baseUri, FileStatus[] status, String curDirName)
			throws FileNotFoundException, IOException {

		Map<String, Object> data = new HashMap<String, Object>();
		data.put("config", config);
		Map<String, HdfsPathInfo> map = new HashMap<String, HdfsPathInfo>();

		for (int i = 0; i < status.length; i++) {
			FileStatus fileStatus = status[i];
			String path = fileStatus.getPath().toString();
			path = path.substring(path.indexOf(baseUri)).substring(baseUri.length());

			int count = 0;

			data.put("length", count);

			if (!fileStatus.isDirectory()) {

				System.out.println("PATH IS " + path);
				System.out.println("ITEMS ARE " + items.keySet());

				ItemDefinition idef = new ItemDefinition();

				idef.setName(path);
				idef.setName(path);
				idef.setData(data);
				idef.setItemType(fileStatus.isDirectory() ? "directory" : "file");
				idef.setType(config.getType());
				idef.setParent(config.getPath());
				items.put(path, idef);
			}

			if (fileStatus.isDirectory()) {

				FileStatus[] subStatus = fileSystem.listStatus(fileStatus.getPath());
				browse(baseUri, subStatus, path);

			} else {
			}
		}

		return items;
	}

	@Override
	public Map<ItemDefinition, List<ItemDefinition>> describe(final String name) throws Exception {

		final Map<ItemDefinition, List<ItemDefinition>> map = new HashMap<ItemDefinition, List<ItemDefinition>>();
		final Map<String, Object> data = new HashMap<String, Object>();
		data.put("config", config);

		fileSystem = FileSystem.get(conf);
		Map<String, ItemDefinition> list = start(config.getPath());
		ItemDefinition idef = new ItemDefinition();
		idef.setName(config.getPath());
		idef.setItemType("directory");
		idef.setType(config.getType());
		idef.setData(data);
		map.put(idef, new ArrayList<ItemDefinition>(list.values()));

		return map;
	}

	public static void main(String[] args) {
		ConnectionConfig c = new ConnectionConfig();
		HdfsConnection hdfs = new HdfsConnection(c);
		try {
			hdfs.start("/user/eperler");

		} catch (IOException e) {
			e.printStackTrace();
		}

	}
}
