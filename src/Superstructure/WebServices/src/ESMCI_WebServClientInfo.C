#include "ESMCI_WebServClientInfo.h"
#include "ESMCI_WebServNetEsmf.h"
#include <string>
#include <iostream>

using namespace std;


/*
*****************************************************************************
**
*****************************************************************************
*/
ClientInfo::ClientInfo(int  clientId)
{
	theClientId = clientId;
	theCurrentStatus = NET_ESMF_STAT_IDLE;
	theServerHost = "";
	theServerPort = 0;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
ClientInfo::~ClientInfo()
{
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ClientInfo::setStatus(int  status)
{
	theCurrentStatus = status;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ClientInfo::setServerHost(string  serverHost)
{
	theServerHost = serverHost;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ClientInfo::setServerPort(int  serverPort)
{
	theServerPort = serverPort;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ClientInfo::addOutputFile(string  outputFile)
{
	theOutputFiles.push_back(outputFile);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ClientInfo::print()
{
	cout << "***** BEGIN ClientInfo *****" << endl;
	cout << "Client ID       : " << theClientId << endl;
	cout << "Status          : " << theCurrentStatus << endl;
	cout << "Server Host     : " << theServerHost << endl;
	cout << "Server Port     : " << theServerPort << endl;
	cout << "*****  END ClientInfo  *****" << endl;
}
