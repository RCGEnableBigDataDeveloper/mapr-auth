package com.mapr.auth.ace;

public enum AceOperator {

	AND("&"), OR("|"), NOT("!");

	private String operator;

	AceOperator(String operator) {
		this.operator = operator;
	}

	public String get() {
		return operator;
	}
}
