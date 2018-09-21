package com.mapr.auth.context;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Properties;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.binary.Hex;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.xml.sax.SAXException;

public class MaprAuthContext {

	private static Properties properties;

	private static final String TMP_SUFFIX = "secret";
	private static final String TMP_PREFIX_SECRETKEY = "key";
	private static final String EMPTY_STRING = "";
	private static final String DES = "DES";
	private static final String DES_ECB_PKCS5_PADDING = "DES/ECB/PKCS5Padding";
	private static final String UTF_8 = "UTF-8";

	static {
		properties = new Properties();
		try {
			properties.load(MaprAuthContext.class.getResourceAsStream("/mapr-auth.properties"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static String getProperty(final String key) {
		return properties.getProperty(key);
	}

	public static String decrypt(SecretKey key, final String encryptedPassword)
			throws NoSuchAlgorithmException, NoSuchPaddingException, InvalidKeyException, IllegalBlockSizeException,
			BadPaddingException, UnsupportedEncodingException {
		Cipher cipher = Cipher.getInstance(DES_ECB_PKCS5_PADDING);
		cipher.init(Cipher.DECRYPT_MODE, key);
		byte[] raw = Base64.decodeBase64(encryptedPassword);
		byte[] stringBytes = cipher.doFinal(raw);
		String decryptedPassword = new String(stringBytes, UTF_8);
		return decryptedPassword;
	}

	public static String decrypt(String secretKeyFileName, String encryptedPassword)
			throws IOException, InvalidKeyException, NoSuchAlgorithmException, NoSuchPaddingException,
			IllegalBlockSizeException, BadPaddingException, ParserConfigurationException, SAXException {
		SecretKey key = readKeyFromFile(secretKeyFileName);

		if (encryptedPassword != null && !EMPTY_STRING.equals(encryptedPassword)) {
			return decrypt(key, encryptedPassword);
		}
		return encryptedPassword;
	}

	public static String decrypt(InputStream is, String encryptedPassword)
			throws IOException, InvalidKeyException, NoSuchAlgorithmException, NoSuchPaddingException,
			IllegalBlockSizeException, BadPaddingException, ParserConfigurationException, SAXException {
		SecretKey key = readKeyFromStream(is);

		if (encryptedPassword != null && !EMPTY_STRING.equals(encryptedPassword)) {
			return decrypt(key, encryptedPassword);
		}
		return encryptedPassword;
	}

	private static SecretKey readKeyFromFile(String file) throws IOException {
		InputStream fis = new FileInputStream(file);
		return getKey(fis);
	}

	private static SecretKey readKeyFromStream(final InputStream fis) throws IOException {
		return getKey(fis);
	}

	private static SecretKey getKey(final InputStream fis) throws IOException {
		final File tempFile = File.createTempFile(TMP_PREFIX_SECRETKEY, TMP_SUFFIX);
		tempFile.deleteOnExit();
		OutputStream fos = new FileOutputStream(tempFile);
		IOUtils.copy(fis, fos);
		String stringKey = new String(FileUtils.readFileToByteArray(tempFile));
		char[] hex = stringKey.toCharArray();
		byte[] encoded;
		try {
			encoded = Hex.decodeHex(hex);
		} catch (DecoderException e) {
			e.printStackTrace();
			return null;
		}
		SecretKey secretKey = new SecretKeySpec(encoded, DES);
		return secretKey;
	}
}
