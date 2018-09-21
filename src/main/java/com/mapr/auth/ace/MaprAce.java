package com.mapr.auth.ace;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;
import org.eclipse.persistence.oxm.annotations.XmlPath;

@XmlRootElement
@XmlType(propOrder = { "name", "access", "expressions" })
public class MaprAce {

	private String name;
	private AceAccessType access;

	@XmlElementWrapper(name = "expressions")
	@XmlElement(name = "expression")
	private List<AceExpression> expressions;

	public MaprAce() {
		expressions = new ArrayList<AceExpression>();
	}

	public MaprAce(final String name, final AceAccessType access, final List<AceExpression> expressions) {
		this.name = name;
		this.access = access;
		this.expressions = expressions;
	}

	@XmlPath("ace/name/text()")
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@XmlPath("ace/access")
	public AceAccessType getAccess() {
		return access;
	}

	public void setAccess(AceAccessType access) {
		this.access = access;
	}

	@XmlPath("aces/expressions")
	public List<AceExpression> getExpressions() {
		return expressions;
	}

	public void setExpressions(List<AceExpression> expressions) {
		this.expressions = expressions;
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
