package com.mapr.auth;

import java.util.List;
import java.util.Map;

public interface MapRConnection {

	public abstract Map<ItemDefinition, List<ItemDefinition>> describe(final String name) throws Exception;

}
