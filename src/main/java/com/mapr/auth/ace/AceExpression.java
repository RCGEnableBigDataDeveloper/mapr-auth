package com.mapr.auth.ace;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlValue;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

public class AceExpression {

	private String type;
	private String value;
	private String operation;
	private String groupName;
	private String groupOperator;
	private int order;

	public AceExpression() {
	}

	public AceExpression(final String type, final String value, final String operation, final String groupName,
			final String groupOperator, final int order) {
		this.type = type;
		this.value = value;
		this.operation = operation;
		this.groupName = groupName;
		this.groupOperator = groupOperator;
		this.order = order;
	}

	@XmlAttribute
	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	@XmlValue
	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	@XmlAttribute
	public String getOperation() {
		return operation;
	}

	public void setOperation(String operation) {
		this.operation = operation;
	}

	@XmlAttribute
	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	@XmlAttribute
	public String getGroupOperator() {
		return groupOperator;
	}

	public void setGroupOperator(String groupOperator) {
		this.groupOperator = groupOperator;
	}

	@XmlAttribute
	public int getOrder() {
		return order;
	}

	public void setOrder(int order) {
		this.order = order;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this);
	}

	@Override
	public boolean equals(Object o) {
		return EqualsBuilder.reflectionEquals(this, o);
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}
}
