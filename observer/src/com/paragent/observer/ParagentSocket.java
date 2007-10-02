package com.paragent.observer;

import java.io.*;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.security.*;
import java.security.cert.*;

import javax.net.ssl.*;



public class ParagentSocket extends Socket
{
	public ParagentSocket(String sHost, int nPort, String computer, String company, String password) throws DisconnectException
	{
		this.sHost = sHost;
		this.nPort = nPort;
		
		this.computer = computer;
		this.company = company;
		this.password = password;
		
		System.out.println("connecting...");
		
		if (!connectSocket()) {
			throw new DisconnectException("Couldn't connect to " + sHost + ":" + nPort); 
		}
		
		System.out.println("connected");
			
		if (!doParagentHandshake()) {
			throw new DisconnectException("Failed to perform handshake with reflector");
		}
		
		System.out.println("handshook");
		
		if (!connectSSL()) {
			throw new DisconnectException("SSL handshake failed");
		}
		System.out.println("SSL connected");
		
		if (!sendPassword()) {
			throw new DisconnectException("Password not accepted");
		}
		
	}
	
	private String computer;
	private String company;
	private String certificate;
	private String password;

	private int			nPort;
	private String		sHost;
	protected Socket	sock;
	protected SSLSocket	sslSock;

	public InputStream getInputStream() throws IOException
	{
		//return sock.getInputStream();
		return sslSock.getInputStream();
	}

	public OutputStream getOutputStream() throws IOException
	{
		//return sock.getOutputStream();
		return sslSock.getOutputStream();
	}
	/*public InputStream getInputStream() throws IOException
	{
		if (sslSock == null) {
			return sock.getInputStream();
		} else {
			return sslSock.getInputStream();
		}
	}

	public OutputStream getOutputStream() throws IOException
	{
		if (sslSock == null) {
			return sock.getOutputStream();
		} else {
			return sslSock.getOutputStream();
		}
	}*/
	
	public void close() {
		try {
			if (sslSock != null && !sslSock.isClosed()) {
				sslSock.close();
			}
			if (!sock.isClosed()) {
				sock.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/*private String calculateChallengeResponse(String sChallenge)
	{
		MessageDigest md5;
		try
		{
			md5 = MessageDigest.getInstance("MD5");
		}
		catch (NoSuchAlgorithmException e)
		{
			// This should never happen. If it does, MD5 somehow ceased to
			// exist.
			return null;
		}

		String sData = sChallenge + sPassword;
		md5.update(sData.getBytes());
		String sResponse = "";
		byte[] rgbMD5 = md5.digest();
		for (int i = 0; i < rgbMD5.length; i++)
		{
			String s = Integer.toHexString(rgbMD5[i] & 0xFF);
			if (s.length() == 1)
				s = "0" + s;
			sResponse += s;
		}

		return sResponse.toUpperCase();
	}*/

	private boolean connectSocket()
	{
		try
		{
			sock = new Socket(sHost, nPort);
			sock.setSoTimeout(10000);
		}
		catch (UnknownHostException ex)
		{
			return false;
		}
		catch (IOException ex)
		{
			return false;
		}

		return sock.isConnected();
	}
	
	private final static String ALGORITHM = "sunx509";
	private final static String PASSWORD = "observer";
	
	private boolean connectSSL() {
		try {
			// http://www.churchillobjects.com/c/11201h.html
			// Local references used for clarity. Their presence
			// here is part of the reason we need to import
			// so many classes.
			KeyManagerFactory kmf;
			KeyManager[] km;
			KeyStore ks;
			TrustManagerFactory tmf;
			TrustManager[] tm;
			SSLContext sslc;

			// Create a keystore that will read the JKS (Java KeyStore)
			// file format which was created by the keytool utility.
			ks = KeyStore.getInstance("JKS");

			// Load the keystore object with the binary keystore file and
			// a byte array representing its password.
			
			
			
			ks.load(new Base64.InputStream(new ByteArrayInputStream("/u3+7QAAAAIAAAABAAAAAQAMc2VjdXJlY2xpZW50AAABEJOOlcQAAAK7MIICtzAO BgorBgEEASoCEQEBBQAEggKj+Z/QvVBn9F+Dez3z/GndEyP1SKR5o9bC2duXy2nB J66pYoCqk6XsZ5phhJ5LWbduPU+KKOcfjHM1QyhebkAkWHMlKDusY0DFXrDm4LXN rtvINIJpBgj8sId5K9VmZymrJFJ2Kts+tXKpLmnIl6CBzex9g1cdfQV+sUZemGTb bP7zXy7ISAVrxhdmB4fjI9c0OuJ+ggocJu82bZdkUR+RWd0ER8VspCoAFCftHqxI chtQBAY0fQ5sh3jMIvDXrn4cbA/gh3jjhIjTFXESAmI2Kc1mxF9NBrPs93BF3PFS ZKKPJl3Q5HNpH2ezN4D1O+2QdV/9ggxjhH5+hfpQz+YA/9Hii1qf9KJ5/h8GmTdS mQemSzP5NaQXXS72OxGWQ9RPo/ioqtIDx4rCHmG8PuZf2VvxRGPa4YCENjx/Ubg/ GLqdn7tUgGsEipN38WeXfiqItde2mUJ0GlDXTtG8UwgqokCuJV1bSK4g25pGV20P Me4wYZarKbg7iLbLkO5t+p5DDDE3OZYQ8v2U+DXQfWzhMJaKNtWKPFgh9AWSLDg1 9InaNOWRKWer+7MAXn9GUdg75fa4LVtJD6rtuQ2214uQIazWPcdIsl531lZZgw2H O9cT5PvtiN5ydiBTiItxU/+/dadXphB0RsOSKuLyhEhTAxjfX2VbejZKgS3WKCM6 afaZ2CebGDj8+iO+DGRogh94ajVnfA7g/6x6V7ke7pViYafOSGQT9iCx+YxtHFKC 4LXYB6zrsltUEpWGFgGzgUJCF6X4OWNLJqvczJRADTQCAMla4nzQx1lTeEScphdK hhMVQCbAJs7gsHp/0m72lk8erMtkMOCe4CtlQ0XU5jdGtZaZOAY88pGtIqyOUWTA DaaJmpGhjdoHU399e7dMDKYpWl97AAAAAQAFWC41MDkAAAJEMIICQDCCAakCBEXH kQQwDQYJKoZIhvcNAQEEBQAwZzELMAkGA1UEBhMCdXMxEDAOBgNVBAgTB1Vua25v d24xEDAOBgNVBAcTB1Vua25vd24xEDAOBgNVBAoTB1Vua25vd24xEDAOBgNVBAsT B1Vua25vd24xEDAOBgNVBAMTB1Vua25vd24wHhcNMDcwMjA1MjAxODEyWhcNMDcw NTA2MjAxODEyWjBnMQswCQYDVQQGEwJ1czEQMA4GA1UECBMHVW5rbm93bjEQMA4G A1UEBxMHVW5rbm93bjEQMA4GA1UEChMHVW5rbm93bjEQMA4GA1UECxMHVW5rbm93 bjEQMA4GA1UEAxMHVW5rbm93bjCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEA pf6nRcLoGHxtn87qE7absvamcOrPejQ1j0GvR4LTFJOhd8QhiD8ehbET/cJYDItr T2vPbofg9LNuOOJ93j5qprvnYePaOXIzZQBroq7YvXLsBqTt5yEVGuw4az1JM8Kf H9I6wgI6XA9fKZs9gqG3SDGHRxZyX/sTvuAjAlbnlf0CAwEAATANBgkqhkiG9w0B AQQFAAOBgQA6wbeMkDCr3kgC5UIzXZ9Vs56fuhKbvVJrZ682oN61/7fYpZzYQ4aN H5yPIM98oQnWpZ1u2OwZPcwCARWrziN4DKPe9nl/VtgGKMrEcWsFuHxIce3sx1/b I4tG+/drqZQUjQOOKJeS3NdhG/KKBdUAvBqh3Y9dKjdfT4ID285I4+pH+bmBLzdU 1nq7el6LeiyeIiuP ".getBytes())),
					PASSWORD.toCharArray());
			

			
			
			// Gives us a factory for key managers that will let
			// us handle the asymetric keys we created earlier.
			kmf = KeyManagerFactory.getInstance(ALGORITHM);

			// Initialize the key manager factory with the keystore object,
			// again using the same password for security since it is going to
			// access the private key.
			kmf.init(ks, PASSWORD.toCharArray());

			// Now we can get the key managers from the factory, since it knows
			// what type we are using now.
			km = kmf.getKeyManagers();
			

			// Next, create a trust manager factory using the same algorithm.
			// This is to avoid using the certificates in cacerts that
			// represent an authentication security risk.
			tmf = TrustManagerFactory.getInstance(ALGORITHM);

			// ...then initialize it with the keystore object. This time we don't
			// need the keystore password. This is because trusted certificates
			// are not a sensitive element in the keystore, unlike the
			// private keys.
			tmf.init(ks);

			// Once that's initialized, get the trust managers from the factory.
			tm = tmf.getTrustManagers();

			// Almost done, we need a context object that will get our
			// server socket factory. We specify TLS to indicate that we will
			// need a server socket factory that supports SSL.
			sslc = SSLContext.getInstance("TLS");

			// Initialize the context object with the key managers and trust
			// managers we got earlier. The third parameter is an optional
			// SecureRandom object. By passing in null, we are letting the
			// context object create its own.
			sslc.init(km, tm, null);
			//sslc.init(null, null, null);
			

			// Finally, we get the ordinary-looking socket factory
			// from the context object.
			SSLSocketFactory ssf = sslc.getSocketFactory();

			// From the factory, we simply ask for an ordinary-looking
			// socket on the port we wish.
			sslSock = (SSLSocket) ssf.createSocket(sock, sHost, nPort, true);
			sslSock.setNeedClientAuth(false);
			sslSock.setUseClientMode(false);
			
			sslSock.startHandshake();
			
			
			return !sslSock.isClosed();
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
		} catch (CertificateException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (NoSuchAlgorithmException e) {
			e.printStackTrace();
		} catch (KeyStoreException e) {
			e.printStackTrace();
		} catch (KeyManagementException e) {
			e.printStackTrace();
		} catch (UnrecoverableKeyException e) {
			e.printStackTrace();
		}
		return false;
	}
	
	
	private boolean sendPassword() {
		try {
			System.out.println("*" + password);
			sslSock.getOutputStream().write((password + "\n").getBytes());
			return !sslSock.isClosed();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return false;
	}

	private String readLine() throws IOException, DisconnectException
	{
		DataInputStream is;
		int cAvailable = 0;

		is = new DataInputStream(new BufferedInputStream(sock.getInputStream(),
				16384));

		// Block on this read, we need a response.
		while (sock.isConnected() && (cAvailable = is.available()) == 0)
		{
			try
			{
				Thread.sleep(100);
			}
			catch (InterruptedException e)
			{}
		}

		if (!sock.isConnected())
		{
			throw new DisconnectException("Disconnected in ReadLine()");
		}

		// Read in until the next newline character, but no more.
		// We don't want to take anything extra off the buffer, RFB may need it.
		byte[] rgb = new byte[cAvailable];
		for (int i = 0; i < cAvailable; i++)
		{
			try
			{
				byte b = is.readByte();
				rgb[i] = b;
				byte[] rgbNewlineTest = { b };
				String sNewlineTest = new String(rgbNewlineTest);
				if (sNewlineTest.equals("\n"))
				{
					break;
				}
			}
			catch (SocketTimeoutException e)
			{
				break;
			}
		}
		return new String(rgb);
	}

	private void writeLine(String sData) throws IOException
	{
		if (!sData.endsWith("\r\n"))
		{
			sData += "\r\n";
		}

		OutputStream os = sock.getOutputStream();
		os.write(sData.getBytes());
	}
	
	protected boolean doParagentHandshake() throws DisconnectException {
		try {
			writeLine("(\"OBSERVER\" \"" + computer + "\" \"" + company + "\")");
			
			String response = readLine();
			System.out.println("<--" + response);
			if (response.startsWith("(GOOD)")) {
				return true;
			}
		} catch (IOException e) {
			return false;
		}
		
		return false;
	}

	/*protected boolean doAardvarkHandshake() throws DisconnectException
	{
		try
		{
			String sResponse;

			writeLine("HELO Aardvark 1.0 en-us");
			sResponse = readLine();
			if (!sResponse.equals("OK Reflector 1.0\r\n"))
			{
				return false;
			}

			writeLine("INVI " + sInvitation + " HELP");
			sResponse = readLine();
			if (!sResponse.startsWith("CHAL "))
			{
				return false;
			}

			String sChalPrefix = "CHAL ";
			String sChallenge = sResponse.substring(sChalPrefix.length(),
					sResponse.indexOf("\r"));
			writeLine("RESP " + calculateChallengeResponse(sChallenge));

			for (;;)
			{
				sResponse = readLine();
				if (sResponse.startsWith("WAIT"))		// This can be sent over and over.
				{
					writeLine("WAITING");
					try
					{
						Thread.sleep(500);
					}
					catch (InterruptedException e)
					{
						continue;
					}
				}
				else if (sResponse.startsWith("GO "))	// Need a space after GO so we don't confuse with GOAWAY.
				{
					break;
				}
			}
		}
		catch (IOException e)
		{
			return false;
		}

		return true;
	}*/
}
