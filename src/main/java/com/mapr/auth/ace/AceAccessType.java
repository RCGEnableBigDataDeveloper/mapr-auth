package com.mapr.auth.ace;

import javax.xml.bind.annotation.XmlAttribute;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

import com.mapr.fs.MapRFileAce;

public class AceAccessType {

	private String path;
	private MapRFileAce.AccessType type;

	public AceAccessType() {
	}

	public AceAccessType(final String path, final MapRFileAce.AccessType type) {
		this.path = path;
		this.type = type;
	}

	@XmlAttribute(name = "type")
	public MapRFileAce.AccessType getType() {
		return type;
	}

	public void setType(MapRFileAce.AccessType type) {
		this.type = type;
	}

	@XmlAttribute(name = "path")
	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
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
