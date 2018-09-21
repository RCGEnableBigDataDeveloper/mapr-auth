package com.mapr.auth.ace;

public enum AceIdentity {

	USER("u"), GROUP("g"), ROLE("r"), PUBLIC("p");

	private String identity;

	AceIdentity(String identity) {
		this.identity = identity;
	}

	public String get() {
		return identity;
	}
}
