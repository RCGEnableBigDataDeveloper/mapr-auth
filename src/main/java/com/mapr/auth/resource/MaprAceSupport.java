package com.mapr.auth.resource;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataOutputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.log4j.Logger;

import com.google.common.base.Throwables;
import com.mapr.auth.ace.AceExpression;
import com.mapr.auth.ace.AceOperator;
import com.mapr.auth.ace.MaprAce;
import com.mapr.fs.MapRFileAce;
import com.mapr.fs.MapRFileSystem;

public class MaprAceSupport {

	private final Logger logger = Logger.getLogger(getClass());

	private final List<MapRFileAce> aces = new ArrayList<>();

	private final FileSystem fs = getFileSystem();

	protected void setMaprAce(final MaprAce ace) {
		try {
			MapRFileAce mapprAce = new MapRFileAce(ace.getAccess().getType());
			String expression = buildExpression(ace);
			logger.info("expression is " + expression);
			mapprAce.setBooleanExpression(expression);
			aces.add(mapprAce);
			((MapRFileSystem) fs).setAces(new Path(ace.getAccess().getPath()), aces);

		} catch (IOException e) {
			logger.error("error occurredin set ace", e);
			logger.error(Throwables.getStackTraceAsString(e));
			e.printStackTrace();
		}
	}

	protected List<MapRFileAce> getMaprAce(final String path) {
		try {
			logger.info("aces requested for path " + path);
			return ((MapRFileSystem) fs).getAces(new Path(path));

		} catch (IOException e) {
			logger.error("error occurred in get ace", e);
			logger.error(Throwables.getStackTraceAsString(e));
			e.printStackTrace();
		}
		return null;
	}

	protected void modifyAce(final MaprAce ace) {

		try {
			MapRFileAce mapprAce = new MapRFileAce(ace.getAccess().getType());
			String expression = buildExpression(ace);
			logger.info("expression is " + expression);
			mapprAce.setBooleanExpression(expression);
			aces.add(mapprAce);
			((MapRFileSystem) fs).modifyAces(new Path(ace.getAccess().getPath()), aces);

		} catch (IOException e) {
			logger.error("error occurred in modify ace", e);
			logger.error(Throwables.getStackTraceAsString(e));
			e.printStackTrace();
		}
	}

	protected void deleteAce(final MaprAce ace) {
		try {
			((MapRFileSystem) fs).deleteAces(new Path(ace.getAccess().getPath()));
		} catch (IOException e) {
			logger.error("error occurred in delete ace", e);
			logger.error(Throwables.getStackTraceAsString(e));
			e.printStackTrace();
		}
	}

	private boolean mkDir(Path path) throws IOException {
		boolean res = fs.mkdirs(path);
		if (!res) {
			throw new IOException("mkdir failed, path: " + path);
		}
		return res;
	}

	private void createFile(Path path) throws Exception {
		byte buf[] = new byte[1024];
		FSDataOutputStream ostr = fs.create(path, true, 512, (short) 1, (long) (64 * 1024 * 1024));
		ostr.write(buf);
		ostr.close();
	}

	private FileSystem getFileSystem() {
		Configuration conf = new Configuration();
		FileSystem fs = null;
		try {
			fs = FileSystem.get(conf);
		} catch (IOException e) {
			logger.error("error occurred filesystem get", e);
			logger.error(Throwables.getStackTraceAsString(e));
			e.printStackTrace();
		}
		return fs;
	}

	protected void makePath(final String dir, final String file) {
		try {
			Path testDir = new Path(dir + "FileAceTest");
			mkDir(testDir);
			Path testFile = new Path(testDir + "/testFile");
			createFile(testFile);
		} catch (Exception e) {
			logger.error("error occurred in makePath", e);
			logger.error(Throwables.getStackTraceAsString(e));
			e.printStackTrace();
		}
	}

	public String buildExpression(final MaprAce ace) {

		Appendable buffer = new StringBuilder();

		try {
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
		} catch (IOException e) {
			logger.error("error occurred in buildExpression", e);
			logger.error(Throwables.getStackTraceAsString(e));
			e.printStackTrace();
		}

		return buffer.toString();
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
}
