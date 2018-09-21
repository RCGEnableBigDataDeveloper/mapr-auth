package com.mapr.auth.ace;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataOutputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;

import com.mapr.fs.MapRFileAce;
import com.mapr.fs.MapRFileSystem;

public final class MaprAceSupport {

	private List<MapRFileAce> aces = new ArrayList<>();

	protected void makePath(final FileSystem fs, final String dir, final String file) {
		try {
			Path testDir = new Path(dir + "FileAceTest");
			mkDir(fs, testDir);
			Path testFile = new Path(testDir + "/testFile");
			createFile(fs, testFile);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	protected void setAce(final FileSystem fs, final Path path) {
		try {
			MapRFileAce ace = new MapRFileAce(MapRFileAce.AccessType.READFILE);
			ace.setBooleanExpression("u:m7user1");
			aces.add(ace);
			ace = new MapRFileAce(MapRFileAce.AccessType.READDIR);
			ace.setBooleanExpression("u:m7user1|u:root");
			aces.add(ace);
			((MapRFileSystem) fs).setAces(path, aces);
			((MapRFileSystem) fs).setAces(path, aces);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	protected List<MapRFileAce> getAces(final FileSystem fs, final Path path) {
		try {
			return ((MapRFileSystem) fs).getAces(path);
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

	protected void modifyAce(final FileSystem fs, final Path path, final String... expressions) {

		try {
			for (String expression : expressions) {

			}
			aces = new ArrayList<MapRFileAce>();
			MapRFileAce ace = new MapRFileAce(MapRFileAce.AccessType.READDIR);
			ace.setBooleanExpression("u:m7user1|u:root|u:m7user2");
			aces.add(ace);
			ace = new MapRFileAce(MapRFileAce.AccessType.WRITEFILE);
			ace.setBooleanExpression("u:m7user2");
			aces.add(ace);
			((MapRFileSystem) fs).modifyAces(path, aces);
			((MapRFileSystem) fs).modifyAces(path, aces);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	protected void deleteAce(final FileSystem fs, final Path path) {
		try {
			((MapRFileSystem) fs).deleteAces(path);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static boolean mkDir(FileSystem fs, Path path) throws IOException {
		boolean res = fs.mkdirs(path);
		if (!res) {
			throw new IOException("mkdir failed, path: " + path);
		}
		return res;
	}

	private static void createFile(FileSystem fs, Path path) throws Exception {
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
			// MapRFileSystem fs = getMapRFileSystem();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return fs;
	}
}
