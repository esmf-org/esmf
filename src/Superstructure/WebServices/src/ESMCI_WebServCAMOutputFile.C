#include "ESMCI_WebServCAMOutputFile.h"

#include <stdlib.h>
#include <math.h>

/*
*****************************************************************************
**
*****************************************************************************
*/
CAMOutputFile::CAMOutputFile(string  filename)
{
	theNetCdfFile = new NcFile(filename.c_str(), NcFile::ReadOnly);

	NcVar*	latVar = theNetCdfFile->get_var("lat");
	NcDim*	latDim = latVar->get_dim(0);
	theNumLatValues = latDim->size();
	theLatValues = new double[theNumLatValues];
	latVar->get(theLatValues, theNumLatValues);
cout << "Lat dim size: " << theNumLatValues << endl;

	NcVar*	lonVar = theNetCdfFile->get_var("lon");
	NcDim*	lonDim = lonVar->get_dim(0);
	theNumLonValues = lonDim->size();
	theLonValues = new double[theNumLonValues];
	lonVar->get(theLonValues, theNumLonValues, 0, 0, 0, 0);
cout << "Lon dim size: " << theNumLonValues << endl;

	NcVar*	timeVar = theNetCdfFile->get_var("time");
	NcDim*	timeDim = timeVar->get_dim(0);
	theNumTimeValues = timeDim->size();
	theTimeValues = new double[theNumTimeValues];
	timeVar->get(theTimeValues, theNumTimeValues, 0, 0, 0, 0);
cout << "Time dim size: " << theNumTimeValues << endl;

	NcVar*	prectVar = theNetCdfFile->get_var("PRECT");
	thePRECTValues = new double[theNumLatValues * theNumLonValues];
	prectVar->get(thePRECTValues, theNumLatValues, theNumLonValues);

	NcVar*	tsVar = theNetCdfFile->get_var("TS");
	theTSValues = new double[theNumLatValues * theNumLonValues];
	tsVar->get(theTSValues, theNumLatValues, theNumLonValues);

	NcVar*	solinVar = theNetCdfFile->get_var("SOLIN");
	theSOLINValues = new double[theNumLatValues * theNumLonValues];
	solinVar->get(theSOLINValues, theNumLatValues, theNumLonValues);

	//***
	// Right now, I'm just getting the first level of relhum... eventually,
	// I'll need to make sure I'm getting the correct level
	//***
	NcVar*	relhumVar = theNetCdfFile->get_var("RELHUM");
	theRELHUMValues = new double[theNumLatValues * theNumLonValues];
	relhumVar->get(theRELHUMValues, 1, 1, theNumLatValues, theNumLonValues);

	NcVar*	ubotVar = theNetCdfFile->get_var("UBOT");
	theUBOTValues = new double[theNumLatValues * theNumLonValues];
	ubotVar->get(theUBOTValues, theNumLatValues, theNumLonValues);

	NcVar*	vbotVar = theNetCdfFile->get_var("VBOT");
	theVBOTValues = new double[theNumLatValues * theNumLonValues];
	vbotVar->get(theVBOTValues, theNumLatValues, theNumLonValues);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
CAMOutputFile::~CAMOutputFile()
{
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  CAMOutputFile::getTimeIndex(double  timeValue)
{
	//***
	// Right now, the model values I'm working with deal with just 1 day in 
	// CAM, so I can assume this value will always be zero.
	// NOTE: This needs to be done properly asap
	//***
	return 0;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  CAMOutputFile::getLatIndex(double  latValue)
{
	int	latIndex = 0;

	if ((latValue < -90)  ||  (latValue > 90))
	{
		// This is not a valid latitude
		return 0;
	}

	//***
	// Right now, I can assume that the lat values are in ascending order, so
	// I just need to find the 2 lats that surround this lat value, and then
	// pick the one that's closer.
	//***
	double	lowValue = theLatValues[0];
	int		valueIdx = 1;

	while ((valueIdx < theNumLatValues)  &&  (theLatValues[valueIdx] < latValue))
	{
		lowValue = theLatValues[valueIdx];
		++valueIdx;
	}

	double	highValue = theLatValues[valueIdx];

	if (abs(latValue - lowValue) > abs(highValue - latValue))
	{
		latIndex = valueIdx;
	}
	else
	{
		latIndex = valueIdx - 1;
	}

	return latIndex;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  CAMOutputFile::getLonIndex(double  lonValue)
{
	int	lonIndex = 0;

	if ((lonValue < 0)  ||  (lonValue > 360))
	{
		// This is not a valid latitude
		return 0;
	}

	//***
	// Right now, I can assume that the lon values are in ascending order, so
	// I just need to find the 2 lons that surround this lon value, and then
	// pick the one that's closer.
	//***
	double	lowValue = theLonValues[0];
	int		valueIdx = 1;

	while ((valueIdx < theNumLonValues)  &&  (theLonValues[valueIdx] < lonValue))
	{
		lowValue = theLonValues[valueIdx];
		++valueIdx;
	}

	//***
	// Because the CAM Output does not use the upper value for longitudes, 
	// which is the same as zero, we have to adjust the algorithm a bit
	//***
	double	highValue = 0;
	if (valueIdx == theNumLonValues)
	{
		highValue = 360;
	}
	else
	{
		highValue = theLonValues[valueIdx];
	}

	if (abs(lonValue - lowValue) > abs(highValue - lonValue))
	{
		if (valueIdx == theNumLonValues)
		{
			lonIndex = 0;
		}
		else
		{
			lonIndex = valueIdx;
		}
	}
	else
	{
		lonIndex = valueIdx - 1;
	}

	return lonIndex;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
double  CAMOutputFile::getDataValue(string  varName,
                                    double  timeValue,
                                    double  latValue,
                                    double  lonValue)
{
	double	retValue = 0.0;

	if (varName.compare("temp") == 0)
	{
		retValue = getAirTemp(timeValue, latValue, lonValue);
	}
	else if (varName.compare("precip") == 0)
	{
		retValue = getPrecip(timeValue, latValue, lonValue);
	}
	else if (varName.compare("windspeed") == 0)
	{
		retValue = getWindSpeed(timeValue, latValue, lonValue);
	}
	else if (varName.compare("solarrad") == 0)
	{
		retValue = getSolarRad(timeValue, latValue, lonValue);
	}
	else if (varName.compare("relhumid") == 0)
	{
		retValue = getRelHumid(timeValue, latValue, lonValue);
	}

	return retValue;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
double  CAMOutputFile::getAirTemp(double  timeValue,
                                  double  latValue,
                                  double  lonValue)
{
	int	latIdx = getLatIndex(latValue);
	int	lonIdx = getLonIndex(lonValue);

	double	tsValue = theTSValues[(latIdx * theNumLonValues) + lonIdx];
	double	retValue = tsValue - 273.15;

	return retValue;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
double  CAMOutputFile::getWindSpeed(double  timeValue,
                                    double  latValue,
                                    double  lonValue)
{
	int	latIdx = getLatIndex(latValue);
	int	lonIdx = getLonIndex(lonValue);

	double	ubotValue = theUBOTValues[(latIdx * theNumLonValues) + lonIdx];
	double	vbotValue = theVBOTValues[(latIdx * theNumLonValues) + lonIdx];
	double	retValue = sqrt(pow(ubotValue, 2) + pow(vbotValue, 2));

	return retValue;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
double  CAMOutputFile::getPrecip(double  timeValue,
                                 double  latValue,
                                 double  lonValue)
{
	int	latIdx = getLatIndex(latValue);
	int	lonIdx = getLonIndex(lonValue);

	double	prectValue = thePRECTValues[(latIdx * theNumLonValues) + lonIdx];
	double	retValue = prectValue * 86400 * 1000;

	return retValue;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
double  CAMOutputFile::getSolarRad(double  timeValue,
                                   double  latValue,
                                   double  lonValue)
{
	int	latIdx = getLatIndex(latValue);
	int	lonIdx = getLonIndex(lonValue);

	double	solinValue = theSOLINValues[(latIdx * theNumLonValues) + lonIdx];
	double	retValue = solinValue * 86400 * 10;

	return retValue;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
double  CAMOutputFile::getRelHumid(double  timeValue,
                                   double  latValue,
                                   double  lonValue)
{
	int	latIdx = getLatIndex(latValue);
	int	lonIdx = getLonIndex(lonValue);

	double	relHumValue = theRELHUMValues[(latIdx * theNumLonValues) + lonIdx];
	double	retValue = relHumValue / 100;

	return retValue;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  CAMOutputFile::printSourceValues()
{
	for (int i = 0; i < theNumLatValues; ++i)
	{
		cout << "Lat value[" << i << "]: " << theLatValues[i] << endl;
	}

	for (int i = 0; i < theNumLonValues; ++i)
	{
		cout << "Lon value[" << i << "]: " << theLonValues[i] << endl;
	}

	for (int i = 0; i < theNumTimeValues; ++i)
	{
		cout << "Time value[" << i << "]: " << theTimeValues[i] << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << thePRECTValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << theTSValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << theSOLINValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << theRELHUMValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << theUBOTValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << theVBOTValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}
}
