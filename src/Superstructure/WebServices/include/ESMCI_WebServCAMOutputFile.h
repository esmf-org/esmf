/*
 *
 */

#ifndef _CAMOutputFile_h_
#define _CAMOutputFile_h_

#include <string>
#include <netcdfcpp.h>

using namespace std;


class CAMOutputFile 
{
public:

	CAMOutputFile(string  filename);
	~CAMOutputFile();

	double  getDataValue(string  varName,
	                     double  timeValue, double  latValue, double  lonValue);
	double  getAirTemp(double  timeValue, double  latValue, double  lonValue);
	double  getWindSpeed(double  timeValue, double  latValue, double  lonValue);
	double  getPrecip(double  timeValue, double  latValue, double  lonValue);
	double  getSolarRad(double  timeValue, double  latValue, double  lonValue);
	double  getRelHumid(double  timeValue, double  latValue, double  lonValue);

private:

	int  getTimeIndex(double  timeValue);
	int  getLatIndex(double  latValue);
	int  getLonIndex(double  lonValue);

	void  printSourceValues();

	NcFile*		theNetCdfFile;

	int		theNumLatValues;
	double*	theLatValues;
	int		theNumLonValues;
	double*	theLonValues;
	int		theNumTimeValues;
	double*	theTimeValues;

	double*	thePRECTValues;
	double*	theTSValues;
	double*	theSOLINValues;
	double*	theRELHUMValues;
	double*	theUBOTValues;
	double*	theVBOTValues;
};


#endif
