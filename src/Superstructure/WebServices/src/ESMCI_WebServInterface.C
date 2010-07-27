#include <stdio.h>
#include <string.h>
#include "ESMCI_WebServInterface.h"
#include "ESMCI_WebServComponentSvr.h"
#include "ESMCI_WebServNetEsmfServer.h"
#include "ESMCI_WebServNetEsmfClient.h"
#include "ESMCI_WebServRegistrarClient.h"
#include "ESMCI_Macros.h"
#include "ESMCI_Comp.h"


void componentsvcloop_(ESMCI::GridComp*   comp,
                       ESMCI::State*      importState,
                       ESMCI::State*      exportState,
                       ESMCI::Clock*      clock,
                       int*               portNum)
{
printf("Port Number: %d\n", *portNum);

	ComponentSvr	server(*portNum);
	server.requestLoop(comp, importState, exportState, clock, 0, ESMF_BLOCKING);
}


void gridserviceloop_(ESMCI::GridComp*   comp,
                      ESMCI::State*      importState,
                      ESMCI::State*      exportState,
                      ESMCI::Clock*      clock,
                      int*               portNum)
{
printf("Port Number: %d\n", *portNum);

	NetEsmfServer	server(*portNum);
	server.requestLoop(comp, importState, exportState, clock, 0, ESMF_BLOCKING);
}


void couplerserviceloop_(ESMCI::CplComp*   comp,
                         ESMCI::State*     importState,
                         ESMCI::State*     exportState,
                         ESMCI::Clock*     clock,
                         int*              portNum)
{
printf("Port Number: %d\n", *portNum);

	NetEsmfServer	server(*portNum);
	server.requestLoop(comp, importState, exportState, clock, 0, ESMF_BLOCKING);
}


void  registercomponent_(char*         params, 
                         unsigned int  param_len)
{
	printf("registerComponent()\n");
	//printf("Params: %s\n", params);
	//printf("Length: %d\n", param_len);

	char	paramStr[param_len + 1];
	strncpy(paramStr, params, param_len);
	paramStr[param_len] = '\0';

	char	name[param_len];
	char	desc[param_len];
	char	hostName[param_len];
	char	portNum[param_len];

	strcpy(name, strtok(paramStr, ":"));
	strcpy(desc, strtok(NULL, ":"));
	strcpy(hostName, strtok(NULL, ":"));
	strcpy(portNum, strtok(NULL, ":"));

	printf("Name: %s\n", name);
	printf("Desc: %s\n", desc);
	printf("Host: %s\n", hostName);
	printf("Port: %s\n", portNum);

	RegistrarClient	client("localhost", REGISTRAR_PORT);

	if (client.connect() < 0)
	{
		return;
	}

	char	response[1024];
	client.registerComp(name, desc, hostName, portNum, response);

	printf("Response: %s\n", response);
	client.disconnect();
}


void  unregistercomponent_(char*         params, 
                           unsigned int  param_len)
{
	printf("unregisterComponent()\n");
	//printf("Params: %s\n", params);
	//printf("Length: %d\n", param_len);

	char	paramStr[param_len + 1];
	strncpy(paramStr, params, param_len);
	paramStr[param_len] = '\0';

	char	name[param_len];
	char	hostName[param_len];
	char	portNum[param_len];

	strcpy(name, strtok(paramStr, ":"));
	strcpy(hostName, strtok(NULL, ":"));
	strcpy(portNum, strtok(NULL, ":"));

	printf("Name: %s\n", name);
	printf("Host: %s\n", hostName);
	printf("Port: %s\n", portNum);

	RegistrarClient	client("localhost", REGISTRAR_PORT);

	if (client.connect() < 0)
	{
		return;
	}

	char	response[1024];
	client.unregisterComp(name, hostName, portNum, response);

	printf("Response: %s\n", response);
	client.disconnect();
}

/*
void remoteinit_(ESMC_GridComp*     comp,
                 ESMC_State*        importState,
                 ESMC_State*        exportState,
                 ESMC_Clock*        clock,
                 int*               portNum,
                 char*              hostName,
                 int                hostNameLen)
{
	char	hostNameStr[hostNameLen + 1];
	strncpy(hostNameStr, hostName, hostNameLen);
	hostNameStr[hostNameLen] = '\0';

	NetEsmfClient	client(hostNameStr, *portNum);
	int				request = NET_ESMF_INIT;
	char*				rquestStr = (char*)"INIT";

	int	n = 0;
	char	s[1024];

	if (client.connect() < 0)
	{
		printf("Error connecting to client\n");
		return;
	}

	client.sendRequest(request, n, s);
	client.getResponse(request, n, s);
	client.disconnect();
}

void remoterun_(ESMC_GridComp*     comp,
                ESMC_State*        importState,
                ESMC_State*        exportState,
                ESMC_Clock*        clock,
                int*               portNum,
                char*              hostName,
                int                hostNameLen)
{
	char	hostNameStr[hostNameLen + 1];
	strncpy(hostNameStr, hostName, hostNameLen);
	hostNameStr[hostNameLen] = '\0';

	NetEsmfClient	client(hostNameStr, *portNum);
	int				request = NET_ESMF_RUN;
	char*				rquestStr = (char*)"RUN";

	int	n = 0;
	char	s[1024];

	if (client.connect() < 0)
	{
		printf("Error connecting to client\n");
		return;
	}

	client.sendRequest(request, n, s);
	client.getResponse(request, n, s);
	client.disconnect();
}

void remotefinal_(ESMC_GridComp*     comp,
                  ESMC_State*        importState,
                  ESMC_State*        exportState,
                  ESMC_Clock*        clock,
                  int*               portNum,
                  char*              hostName,
                  int                hostNameLen)
{
	char	hostNameStr[hostNameLen + 1];
	strncpy(hostNameStr, hostName, hostNameLen);
	hostNameStr[hostNameLen] = '\0';

	NetEsmfClient	client(hostNameStr, *portNum);
	int				request = NET_ESMF_FINAL;
	char*				rquestStr = (char*)"FINAL";

	int	n = 0;
	char	s[1024];

	if (client.connect() < 0)
	{
		printf("Error connecting to client\n");
		return;
	}

	client.sendRequest(request, n, s);
	client.getResponse(request, n, s);
	client.disconnect();
}
*/
