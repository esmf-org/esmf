/*
 *
 */

#ifndef _ClientInfo_h_
#define _ClientInfo_h_

#include <vector>
#include <string>

using namespace std;

class ClientInfo
{
public:

	ClientInfo(int  clientId);
	~ClientInfo();

	int    				clientId()		{ return theClientId; }
	int    				status()			{ return theCurrentStatus; }
	string    			serverHost()	{ return theServerHost; }
	int    				serverPort()	{ return theServerPort; }
	vector<string>  	outputFiles()	{ return theOutputFiles; }

   void  setStatus(int  status);
	void  setServerHost(string  serverHost);
	void  setServerPort(int  serverPort);
	void  addOutputFile(string  outputFile);

   void  print();

private:

	int				theClientId;
	int				theCurrentStatus;
	string			theServerHost;
	int				theServerPort;
	vector<string>	theOutputFiles;
};


#endif
