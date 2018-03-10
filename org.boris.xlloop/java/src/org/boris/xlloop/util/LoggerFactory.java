package org.boris.xlloop.util;

import java.io.StringWriter;

public class LoggerFactory {

	private static final String CONFIG_PARAM_NAME = "xlloop.log.configuration";
	
	private static LoggerFactory instance = new LoggerFactory();
	
	private boolean loggingEnabled;
	
	public static LoggerFactory getLogger() {
		return instance;
	}

	private LoggerFactory() {
		String val = System.getProperty(CONFIG_PARAM_NAME);
    	
		loggingEnabled = (val != null && "true".equalsIgnoreCase(val));
	}
	
	public void log(StringWriter write) {
		if(!loggingEnabled){
			return;
		}
		System.out.println(write.toString());
	}
	
	public void log(String logStr) {
		if(!loggingEnabled){
			return;
		}
		System.out.println(logStr);
	}
}
