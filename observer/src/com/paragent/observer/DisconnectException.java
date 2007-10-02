package com.paragent.observer;

public class DisconnectException extends Exception
{
	private static final long	serialVersionUID	= 7135130742139477558L;

	public DisconnectException(String error)
	{
		super(error);
	}
}
