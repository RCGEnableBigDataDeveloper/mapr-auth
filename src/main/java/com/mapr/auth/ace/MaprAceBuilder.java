package com.mapr.auth.ace;

import java.util.List;
import java.util.function.Consumer;

public class MaprAceBuilder {

	public String name;
	public String operator;
	public AceAccessType access;
	public List<AceExpression> expressions;

	public MaprAceBuilder with(Consumer<MaprAceBuilder> builderFunction) {
		builderFunction.accept(this);
		return this;
	}

	public MaprAce build() {
		return new MaprAce(name, access, expressions);
	}
}
