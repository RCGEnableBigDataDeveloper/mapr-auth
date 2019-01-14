package com.uhg.mapr.hdfs;

import java.util.List;
import java.util.Map;

public interface DataLakeConnection {

	public abstract Map<ItemDefinition, List<ItemDefinition>> describe(final String name) throws Exception;

}
