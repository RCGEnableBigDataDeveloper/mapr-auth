package com.mapr.auth.ace;

import javax.xml.bind.annotation.XmlAttribute;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import com.mapr.fs.MapRFileAce;

public class AceAccessType {

	private long createdAt;
	private MapRFileAce.AccessType type;

	public AceAccessType() {
	}

	public AceAccessType(final long createdAt, final MapRFileAce.AccessType type) {
		this.createdAt = createdAt;
		this.type = type;
	}

	@XmlAttribute(name = "type")
	public MapRFileAce.AccessType getType() {
		return type;
	}

	public void setType(MapRFileAce.AccessType type) {
		this.type = type;
	}

	@XmlAttribute(name = "createdAt")
	public long getCreatedAt() {
		return createdAt;
	}

	public void setCreatedAt(long createdAt) {
		this.createdAt = createdAt;
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
