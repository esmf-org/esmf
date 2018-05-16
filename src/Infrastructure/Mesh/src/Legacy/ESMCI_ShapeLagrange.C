// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Legacy/ESMCI_ShapeLagrange.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <iterator>
#include <iostream>

#include <vector>
#include <cstdio>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

/**
 * This function provided by Amik St-Cyr, NCAR.  Not sure what that means
 * in terms of the copyright, but certainly ESMF can not claim any special
 * priveledge to this routine.
 */
void lobatto_set ( int order, double *xtab, double *weight )

//******************************************************************************
//
//  Purpose:
//
//    LOBATTO_SET sets abscissas and weights for Lobatto quadrature.
//
//  Discussion:
//
//    The integration interval is [ -1, 1 ].
//
//    The weight function is w(x-1] = 1.0.
//
//    The integral to approximate:
//
//      Integral ( -1 <= X <= 1 ) F(X) dX
//
//    The quadrature rule:
//
//      Sum ( 1 <= I <= ORDER ) WEIGHt[I) * F ( XTAb[I) )
//
//    The quadrature rule will integrate exactly all polynomials up to
//    X**(2*ORDER-3).
//
//    The Lobatto rule is distinguished by the fact that both endpoints
//    (-1 and 1) are always abscissas of the rule.
//
//  Modified:
//
//    30 April 2006
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Milton Abramowitz and Irene Stegun,
//    Handbook of Mathematical Functions,
//    National Bureau of Standards, 1964.
//
//    Arthur Stroud and Don Secrest,
//    Gaussian Quadrature Formulas,
//    Prentice Hall, 1966.
//
//    Daniel Zwillinger, editor,
//    Standard Mathematical Tables and Formulae,
//    30th Edition,
//    CRC Press, 1996.
//
//  Parameters:
//
//    Input, int ORDER, the order of the rule.
//    ORDER must be between 2 and 20.
//
//    Output, double XTAB[ORDER], the abscissas for the rule.
//
//    Output, double WEIGHT[ORDER], the weights of the rule.
//    The weights are positive, symmetric and should sum to 2.
//
{
  if ( order == 2 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =    1.0E+00;

    weight[1-1] =  1.0E+00;
    weight[2-1] =  1.0E+00;
  }
  else if ( order == 3 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =    0.0E+00;
    xtab[3-1] =    1.0E+00;

    weight[1-1] =  1.0 / 3.0E+00;
    weight[2-1] =  4.0 / 3.0E+00;
    weight[3-1] =  1.0 / 3.0E+00;
  }
  else if ( order == 4 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.447213595499957939281834733746E+00;
    xtab[3-1] =    0.447213595499957939281834733746E+00;
    xtab[4-1] =    1.0E+00;

    weight[1-1] =  1.0E+00 / 6.0E+00;
    weight[2-1] =  5.0E+00 / 6.0E+00;
    weight[3-1] =  5.0E+00 / 6.0E+00;
    weight[4-1] =  1.0E+00 / 6.0E+00;
  }
  else if ( order == 5 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.654653670707977143798292456247E+00;
    xtab[3-1] =    0.0E+00;
    xtab[4-1] =    0.654653670707977143798292456247E+00;
    xtab[5-1] =    1.0E+00;

    weight[1-1] =  9.0E+00 / 90.0E+00;
    weight[2-1] = 49.0E+00 / 90.0E+00;
    weight[3-1] = 64.0E+00 / 90.0E+00;
    weight[4-1] = 49.0E+00 / 90.0E+00;
    weight[5-1] =  9.0E+00 / 90.0E+00;
  }
  else if ( order == 6 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.765055323929464692851002973959E+00;
    xtab[3-1] =  - 0.285231516480645096314150994041E+00;
    xtab[4-1] =    0.285231516480645096314150994041E+00;
    xtab[5-1] =    0.765055323929464692851002973959E+00;
    xtab[6-1] =    1.0E+00;

    weight[1-1] =  0.066666666666666666666666666667E+00;
    weight[2-1] =  0.378474956297846980316612808212E+00;
    weight[3-1] =  0.554858377035486353016720525121E+00;
    weight[4-1] =  0.554858377035486353016720525121E+00;
    weight[5-1] =  0.378474956297846980316612808212E+00;
    weight[6-1] =  0.066666666666666666666666666667E+00;
  }
  else if ( order == 7 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.830223896278566929872032213967E+00;
    xtab[3-1] =  - 0.468848793470714213803771881909E+00;
    xtab[4-1] =    0.0E+00;
    xtab[5-1] =    0.468848793470714213803771881909E+00;
    xtab[6-1] =    0.830223896278566929872032213967E+00;
    xtab[7-1] =    1.0E+00;

    weight[1-1] =  0.476190476190476190476190476190E-01;
    weight[2-1] =  0.276826047361565948010700406290E+00;
    weight[3-1] =  0.431745381209862623417871022281E+00;
    weight[4-1] =  0.487619047619047619047619047619E+00;
    weight[5-1] =  0.431745381209862623417871022281E+00;
    weight[6-1] =  0.276826047361565948010700406290E+00;
    weight[7-1] =  0.476190476190476190476190476190E-01;
  }
  else if ( order == 8 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.871740148509606615337445761221E+00;
    xtab[3-1] =  - 0.591700181433142302144510731398E+00;
    xtab[4-1] =  - 0.209299217902478868768657260345E+00;
    xtab[5-1] =    0.209299217902478868768657260345E+00;
    xtab[6-1] =    0.591700181433142302144510731398E+00;
    xtab[7-1] =    0.871740148509606615337445761221E+00;
    xtab[8-1] =    1.0E+00;

    weight[1-1] =  0.357142857142857142857142857143E-01;
    weight[2-1] =  0.210704227143506039382991065776E+00;
    weight[3-1] =  0.341122692483504364764240677108E+00;
    weight[4-1] =  0.412458794658703881567052971402E+00;
    weight[5-1] =  0.412458794658703881567052971402E+00;
    weight[6-1] =  0.341122692483504364764240677108E+00;
    weight[7-1] =  0.210704227143506039382991065776E+00;
    weight[8-1] =  0.357142857142857142857142857143E-01;
  }
  else if ( order == 9 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.899757995411460157312345244418E+00;
    xtab[3-1] =  - 0.677186279510737753445885427091E+00;
    xtab[4-1] =  - 0.363117463826178158710752068709E+00;
    xtab[5-1] =    0.0E+00;
    xtab[6-1] =    0.363117463826178158710752068709E+00;
    xtab[7-1] =    0.677186279510737753445885427091E+00;
    xtab[8-1] =    0.899757995411460157312345244418E+00;
    xtab[9-1] =    1.0E+00;

    weight[1-1] =  0.277777777777777777777777777778E-01;
    weight[2-1] =  0.165495361560805525046339720029E+00;
    weight[3-1] =  0.274538712500161735280705618579E+00;
    weight[4-1] =  0.346428510973046345115131532140E+00;
    weight[5-1] =  0.371519274376417233560090702948E+00;
    weight[6-1] =  0.346428510973046345115131532140E+00;
    weight[7-1] =  0.274538712500161735280705618579E+00;
    weight[8-1] =  0.165495361560805525046339720029E+00;
    weight[9-1] =  0.277777777777777777777777777778E-01;
  }
  else if ( order == 10 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.919533908166458813828932660822E+00;
    xtab[3-1] =  - 0.738773865105505075003106174860E+00;
    xtab[4-1] =  - 0.477924949810444495661175092731E+00;
    xtab[5-1] =  - 0.165278957666387024626219765958E+00;
    xtab[6-1] =    0.165278957666387024626219765958E+00;
    xtab[7-1] =    0.477924949810444495661175092731E+00;
    xtab[8-1] =    0.738773865105505075003106174860E+00;
    xtab[9-1] =    0.919533908166458813828932660822E+00;
    xtab[10-1] =   1.0E+00;

    weight[1-1] =  0.222222222222222222222222222222E-01;
    weight[2-1] =  0.133305990851070111126227170755E+00;
    weight[3-1] =  0.224889342063126452119457821731E+00;
    weight[4-1] =  0.292042683679683757875582257374E+00;
    weight[5-1] =  0.327539761183897456656510527917E+00;
    weight[6-1] =  0.327539761183897456656510527917E+00;
    weight[7-1] =  0.292042683679683757875582257374E+00;
    weight[8-1] =  0.224889342063126452119457821731E+00;
    weight[9-1] =  0.133305990851070111126227170755E+00;
    weight[10-1] = 0.222222222222222222222222222222E-01;
  }
  else if ( order == 11 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.934001430408059134332274136099E+00;
    xtab[3-1] =  - 0.784483473663144418622417816108E+00;
    xtab[4-1] =  - 0.565235326996205006470963969478E+00;
    xtab[5-1] =  - 0.295758135586939391431911515559E+00;
    xtab[6-1] =    0.0E+00;
    xtab[7-1] =    0.295758135586939391431911515559E+00;
    xtab[8-1] =    0.565235326996205006470963969478E+00;
    xtab[9-1] =    0.784483473663144418622417816108E+00;
    xtab[10-1] =   0.934001430408059134332274136099E+00;
    xtab[11-1] =   1.0E+00;

    weight[1-1] =  0.181818181818181818181818181818E-01;
    weight[2-1] =  0.109612273266994864461403449580E+00;
    weight[3-1] =  0.187169881780305204108141521899E+00;
    weight[4-1] =  0.248048104264028314040084866422E+00;
    weight[5-1] =  0.286879124779008088679222403332E+00;
    weight[6-1] =  0.300217595455690693785931881170E+00;
    weight[7-1] =  0.286879124779008088679222403332E+00;
    weight[8-1] =  0.248048104264028314040084866422E+00;
    weight[9-1] =  0.187169881780305204108141521899E+00;
    weight[10-1] = 0.109612273266994864461403449580E+00;
    weight[11-1] = 0.181818181818181818181818181818E-01;
  }
  else if ( order == 12 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.944899272222882223407580138303E+00;
    xtab[3-1] =  - 0.819279321644006678348641581717E+00;
    xtab[4-1] =  - 0.632876153031869677662404854444E+00;
    xtab[5-1] =  - 0.399530940965348932264349791567E+00;
    xtab[6-1] =  - 0.136552932854927554864061855740E+00;
    xtab[7-1] =    0.136552932854927554864061855740E+00;
    xtab[8-1] =    0.399530940965348932264349791567E+00;
    xtab[9-1] =    0.632876153031869677662404854444E+00;
    xtab[10-1] =   0.819279321644006678348641581717E+00;
    xtab[11-1] =   0.944899272222882223407580138303E+00;
    xtab[12-1] =   1.0E+00;

    weight[1-1] =  0.151515151515151515151515151515E-01;
    weight[2-1] =  0.916845174131961306683425941341E-01;
    weight[3-1] =  0.157974705564370115164671062700E+00;
    weight[4-1] =  0.212508417761021145358302077367E+00;
    weight[5-1] =  0.251275603199201280293244412148E+00;
    weight[6-1] =  0.271405240910696177000288338500E+00;
    weight[7-1] =  0.271405240910696177000288338500E+00;
    weight[8-1] =  0.251275603199201280293244412148E+00;
    weight[9-1] =  0.212508417761021145358302077367E+00;
    weight[10-1] = 0.157974705564370115164671062700E+00;
    weight[11-1] = 0.916845174131961306683425941341E-01;
    weight[12-1] = 0.151515151515151515151515151515E-01;
  }
  else if ( order == 13 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.953309846642163911896905464755E+00;
    xtab[3-1] =  - 0.846347564651872316865925607099E+00;
    xtab[4-1] =  - 0.686188469081757426072759039566E+00;
    xtab[5-1] =  - 0.482909821091336201746937233637E+00;
    xtab[6-1] =  - 0.249286930106239992568673700374E+00;
    xtab[7-1] =    0.0E+00;
    xtab[8-1] =    0.249286930106239992568673700374E+00;
    xtab[9-1] =    0.482909821091336201746937233637E+00;
    xtab[10-1] =   0.686188469081757426072759039566E+00;
    xtab[11-1] =   0.846347564651872316865925607099E+00;
    xtab[12-1] =   0.953309846642163911896905464755E+00;
    xtab[13-1] =   1.0E+00;

    weight[1-1] =  0.128205128205128205128205128205E-01;
    weight[2-1] =  0.778016867468189277935889883331E-01;
    weight[3-1] =  0.134981926689608349119914762589E+00;
    weight[4-1] =  0.183646865203550092007494258747E+00;
    weight[5-1] =  0.220767793566110086085534008379E+00;
    weight[6-1] =  0.244015790306676356458578148360E+00;
    weight[7-1] =  0.251930849333446736044138641541E+00;
    weight[8-1] =  0.244015790306676356458578148360E+00;
    weight[9-1] =  0.220767793566110086085534008379E+00;
    weight[10-1] = 0.183646865203550092007494258747E+00;
    weight[11-1] = 0.134981926689608349119914762589E+00;
    weight[12-1] = 0.778016867468189277935889883331E-01;
    weight[13-1] = 0.128205128205128205128205128205E-01;
  }
  else if ( order == 14 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.959935045267260901355100162015E+00;
    xtab[3-1] =  - 0.867801053830347251000220202908E+00;
    xtab[4-1] =  - 0.728868599091326140584672400521E+00;
    xtab[5-1] =  - 0.550639402928647055316622705859E+00;
    xtab[6-1] =  - 0.342724013342712845043903403642E+00;
    xtab[7-1] =  - 0.116331868883703867658776709736E+00;
    xtab[8-1] =    0.116331868883703867658776709736E+00;
    xtab[9-1] =    0.342724013342712845043903403642E+00;
    xtab[10-1] =   0.550639402928647055316622705859E+00;
    xtab[11-1] =   0.728868599091326140584672400521E+00;
    xtab[12-1] =   0.867801053830347251000220202908E+00;
    xtab[13-1] =   0.959935045267260901355100162015E+00;
    xtab[14-1] =   1.0E+00;

    weight[1-1] =  0.109890109890109890109890109890E-01;
    weight[2-1] =  0.668372844976812846340706607461E-01;
    weight[3-1] =  0.116586655898711651540996670655E+00;
    weight[4-1] =  0.160021851762952142412820997988E+00;
    weight[5-1] =  0.194826149373416118640331778376E+00;
    weight[6-1] =  0.219126253009770754871162523954E+00;
    weight[7-1] =  0.231612794468457058889628357293E+00;
    weight[8-1] =  0.231612794468457058889628357293E+00;
    weight[9-1] =  0.219126253009770754871162523954E+00;
    weight[10-1] = 0.194826149373416118640331778376E+00;
    weight[11-1] = 0.160021851762952142412820997988E+00;
    weight[12-1] = 0.116586655898711651540996670655E+00;
    weight[13-1] = 0.668372844976812846340706607461E-01;
    weight[14-1] = 0.109890109890109890109890109890E-01;
  }
  else if ( order == 15 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.965245926503838572795851392070E+00;
    xtab[3-1] =  - 0.885082044222976298825401631482E+00;
    xtab[4-1] =  - 0.763519689951815200704118475976E+00;
    xtab[5-1] =  - 0.606253205469845711123529938637E+00;
    xtab[6-1] =  - 0.420638054713672480921896938739E+00;
    xtab[7-1] =  - 0.215353955363794238225679446273E+00;
    xtab[8-1] =    0.0E+00;
    xtab[9-1] =    0.215353955363794238225679446273E+00;
    xtab[10-1] =   0.420638054713672480921896938739E+00;
    xtab[11-1] =   0.606253205469845711123529938637E+00;
    xtab[12-1] =   0.763519689951815200704118475976E+00;
    xtab[13-1] =   0.885082044222976298825401631482E+00;
    xtab[14-1] =   0.965245926503838572795851392070E+00;
    xtab[15-1] =   1.0E+00;

    weight[1-1] =  0.952380952380952380952380952381E-02;
    weight[2-1] =  0.580298930286012490968805840253E-01;
    weight[3-1] =  0.101660070325718067603666170789E+00;
    weight[4-1] =  0.140511699802428109460446805644E+00;
    weight[5-1] =  0.172789647253600949052077099408E+00;
    weight[6-1] =  0.196987235964613356092500346507E+00;
    weight[7-1] =  0.211973585926820920127430076977E+00;
    weight[8-1] =  0.217048116348815649514950214251E+00;
    weight[9-1] =  0.211973585926820920127430076977E+00;
    weight[10-1] = 0.196987235964613356092500346507E+00;
    weight[11-1] = 0.172789647253600949052077099408E+00;
    weight[12-1] = 0.140511699802428109460446805644E+00;
    weight[13-1] = 0.101660070325718067603666170789E+00;
    weight[14-1] = 0.580298930286012490968805840253E-01;
    weight[15-1] = 0.952380952380952380952380952381E-02;
  }
  else if ( order == 16 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.969568046270217932952242738367E+00;
    xtab[3-1] =  - 0.899200533093472092994628261520E+00;
    xtab[4-1] =  - 0.792008291861815063931088270963E+00;
    xtab[5-1] =  - 0.652388702882493089467883219641E+00;
    xtab[6-1] =  - 0.486059421887137611781890785847E+00;
    xtab[7-1] =  - 0.299830468900763208098353454722E+00;
    xtab[8-1] =  - 0.101326273521949447843033005046E+00;
    xtab[9-1] =    0.101326273521949447843033005046E+00;
    xtab[10-1] =   0.299830468900763208098353454722E+00;
    xtab[11-1] =   0.486059421887137611781890785847E+00;
    xtab[12-1] =   0.652388702882493089467883219641E+00;
    xtab[13-1] =   0.792008291861815063931088270963E+00;
    xtab[14-1] =   0.899200533093472092994628261520E+00;
    xtab[15-1] =   0.969568046270217932952242738367E+00;
    xtab[16-1] =   1.0E+00;

    weight[1-1] =  0.833333333333333333333333333333E-02;
    weight[2-1] =  0.508503610059199054032449195655E-01;
    weight[3-1] =  0.893936973259308009910520801661E-01;
    weight[4-1] =  0.124255382132514098349536332657E+00;
    weight[5-1] =  0.154026980807164280815644940485E+00;
    weight[6-1] =  0.177491913391704125301075669528E+00;
    weight[7-1] =  0.193690023825203584316913598854E+00;
    weight[8-1] =  0.201958308178229871489199125411E+00;
    weight[9-1] =  0.201958308178229871489199125411E+00;
    weight[10-1] = 0.193690023825203584316913598854E+00;
    weight[11-1] = 0.177491913391704125301075669528E+00;
    weight[12-1] = 0.154026980807164280815644940485E+00;
    weight[13-1] = 0.124255382132514098349536332657E+00;
    weight[14-1] = 0.893936973259308009910520801661E-01;
    weight[15-1] = 0.508503610059199054032449195655E-01;
    weight[16-1] = 0.833333333333333333333333333333E-02;
  }
  else if ( order == 17 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.973132176631418314156979501874E+00;
    xtab[3-1] =  - 0.910879995915573595623802506398E+00;
    xtab[4-1] =  - 0.815696251221770307106750553238E+00;
    xtab[5-1] =  - 0.691028980627684705394919357372E+00;
    xtab[6-1] =  - 0.541385399330101539123733407504E+00;
    xtab[7-1] =  - 0.372174433565477041907234680735E+00;
    xtab[8-1] =  - 0.189511973518317388304263014753E+00;
    xtab[9-1] =    0.0E+00;
    xtab[10-1] =   0.189511973518317388304263014753E+00;
    xtab[11-1] =   0.372174433565477041907234680735E+00;
    xtab[12-1] =   0.541385399330101539123733407504E+00;
    xtab[13-1] =   0.691028980627684705394919357372E+00;
    xtab[14-1] =   0.815696251221770307106750553238E+00;
    xtab[15-1] =   0.910879995915573595623802506398E+00;
    xtab[16-1] =   0.973132176631418314156979501874E+00;
    xtab[17-1] =   1.0E+00;

    weight[1-1] =  0.735294117647058823529411764706E-02;
    weight[2-1] =  0.449219405432542096474009546232E-01;
    weight[3-1] =  0.791982705036871191902644299528E-01;
    weight[4-1] =  0.110592909007028161375772705220E+00;
    weight[5-1] =  0.137987746201926559056201574954E+00;
    weight[6-1] =  0.160394661997621539516328365865E+00;
    weight[7-1] =  0.177004253515657870436945745363E+00;
    weight[8-1] =  0.187216339677619235892088482861E+00;
    weight[9-1] =  0.190661874753469433299407247028E+00;
    weight[10-1] = 0.187216339677619235892088482861E+00;
    weight[11-1] = 0.177004253515657870436945745363E+00;
    weight[12-1] = 0.160394661997621539516328365865E+00;
    weight[13-1] = 0.137987746201926559056201574954E+00;
    weight[14-1] = 0.110592909007028161375772705220E+00;
    weight[15-1] = 0.791982705036871191902644299528E-01;
    weight[16-1] = 0.449219405432542096474009546232E-01;
    weight[17-1] = 0.735294117647058823529411764706E-02;
  }
  else if ( order == 18 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.976105557412198542864518924342E+00;
    xtab[3-1] =  - 0.920649185347533873837854625431E+00;
    xtab[4-1] =  - 0.835593535218090213713646362328E+00;
    xtab[5-1] =  - 0.723679329283242681306210365302E+00;
    xtab[6-1] =  - 0.588504834318661761173535893194E+00;
    xtab[7-1] =  - 0.434415036912123975342287136741E+00;
    xtab[8-1] =  - 0.266362652878280984167665332026E+00;
    xtab[9-1] =  - 0.897490934846521110226450100886E-01;
    xtab[10-1] =   0.897490934846521110226450100886E-01;
    xtab[11-1] =   0.266362652878280984167665332026E+00;
    xtab[12-1] =   0.434415036912123975342287136741E+00;
    xtab[13-1] =   0.588504834318661761173535893194E+00;
    xtab[14-1] =   0.723679329283242681306210365302E+00;
    xtab[15-1] =   0.835593535218090213713646362328E+00;
    xtab[16-1] =   0.920649185347533873837854625431E+00;
    xtab[17-1] =   0.976105557412198542864518924342E+00;
    xtab[18-1] =   1.0E+00;

    weight[1-1] =  0.653594771241830065359477124183E-02;
    weight[2-1] =  0.399706288109140661375991764101E-01;
    weight[3-1] =  0.706371668856336649992229601678E-01;
    weight[4-1] =  0.990162717175028023944236053187E-01;
    weight[5-1] =  0.124210533132967100263396358897E+00;
    weight[6-1] =  0.145411961573802267983003210494E+00;
    weight[7-1] =  0.161939517237602489264326706700E+00;
    weight[8-1] =  0.173262109489456226010614403827E+00;
    weight[9-1] =  0.179015863439703082293818806944E+00;
    weight[10-1] = 0.179015863439703082293818806944E+00;
    weight[11-1] = 0.173262109489456226010614403827E+00;
    weight[12-1] = 0.161939517237602489264326706700E+00;
    weight[13-1] = 0.145411961573802267983003210494E+00;
    weight[14-1] = 0.124210533132967100263396358897E+00;
    weight[15-1] = 0.990162717175028023944236053187E-01;
    weight[16-1] = 0.706371668856336649992229601678E-01;
    weight[17-1] = 0.399706288109140661375991764101E-01;
    weight[18-1] = 0.653594771241830065359477124183E-02;
  }
  else if ( order == 19 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.978611766222080095152634063110E+00;
    xtab[3-1] =  - 0.928901528152586243717940258797E+00;
    xtab[4-1] =  - 0.852460577796646093085955970041E+00;
    xtab[5-1] =  - 0.751494202552613014163637489634E+00;
    xtab[6-1] =  - 0.628908137265220497766832306229E+00;
    xtab[7-1] =  - 0.488229285680713502777909637625E+00;
    xtab[8-1] =  - 0.333504847824498610298500103845E+00;
    xtab[9-1] =  - 0.169186023409281571375154153445E+00;
    xtab[10-1] =   0.0E+00;
    xtab[11-1] =   0.169186023409281571375154153445E+00;
    xtab[12-1] =   0.333504847824498610298500103845E+00;
    xtab[13-1] =   0.488229285680713502777909637625E+00;
    xtab[14-1] =   0.628908137265220497766832306229E+00;
    xtab[15-1] =   0.751494202552613014163637489634E+00;
    xtab[16-1] =   0.852460577796646093085955970041E+00;
    xtab[17-1] =   0.928901528152586243717940258797E+00;
    xtab[18-1] =   0.978611766222080095152634063110E+00;
    xtab[19-1] =   1.0E+00;

    weight[1-1] =  0.584795321637426900584795321637E-02;
    weight[2-1] =  0.357933651861764771154255690351E-01;
    weight[3-1] =  0.633818917626297368516956904183E-01;
    weight[4-1] =  0.891317570992070844480087905562E-01;
    weight[5-1] =  0.112315341477305044070910015464E+00;
    weight[6-1] =  0.132267280448750776926046733910E+00;
    weight[7-1] =  0.148413942595938885009680643668E+00;
    weight[8-1] =  0.160290924044061241979910968184E+00;
    weight[9-1] =  0.167556584527142867270137277740E+00;
    weight[10-1] = 0.170001919284827234644672715617E+00;
    weight[11-1] = 0.167556584527142867270137277740E+00;
    weight[12-1] = 0.160290924044061241979910968184E+00;
    weight[13-1] = 0.148413942595938885009680643668E+00;
    weight[14-1] = 0.132267280448750776926046733910E+00;
    weight[15-1] = 0.112315341477305044070910015464E+00;
    weight[16-1] = 0.891317570992070844480087905562E-01;
    weight[17-1] = 0.633818917626297368516956904183E-01;
    weight[18-1] = 0.357933651861764771154255690351E-01;
    weight[19-1] = 0.584795321637426900584795321637E-02;
  }
  else if ( order == 20 )
  {
    xtab[1-1] =  - 1.0E+00;
    xtab[2-1] =  - 0.980743704893914171925446438584E+00;
    xtab[3-1] =  - 0.935934498812665435716181584931E+00;
    xtab[4-1] =  - 0.866877978089950141309847214616E+00;
    xtab[5-1] =  - 0.775368260952055870414317527595E+00;
    xtab[6-1] =  - 0.663776402290311289846403322971E+00;
    xtab[7-1] =  - 0.534992864031886261648135961829E+00;
    xtab[8-1] =  - 0.392353183713909299386474703816E+00;
    xtab[9-1] =  - 0.239551705922986495182401356927E+00;
    xtab[10-1] = - 0.805459372388218379759445181596E-01;
    xtab[11-1] =   0.805459372388218379759445181596E-01;
    xtab[12-1] =   0.239551705922986495182401356927E+00;
    xtab[13-1] =   0.392353183713909299386474703816E+00;
    xtab[14-1] =   0.534992864031886261648135961829E+00;
    xtab[15-1] =   0.663776402290311289846403322971E+00;
    xtab[16-1] =   0.775368260952055870414317527595E+00;
    xtab[17-1] =   0.866877978089950141309847214616E+00;
    xtab[18-1] =   0.935934498812665435716181584931E+00;
    xtab[19-1] =   0.980743704893914171925446438584E+00;
    xtab[20-1] =   1.0E+00;

    weight[1-1] =  0.526315789473684210526315789474E-02;
    weight[2-1] =  0.322371231884889414916050281173E-01;
    weight[3-1] =  0.571818021275668260047536271732E-01;
    weight[4-1] =  0.806317639961196031447768461137E-01;
    weight[5-1] =  0.101991499699450815683781205733E+00;
    weight[6-1] =  0.120709227628674725099429705002E+00;
    weight[7-1] =  0.136300482358724184489780792989E+00;
    weight[8-1] =  0.148361554070916825814713013734E+00;
    weight[9-1] =  0.156580102647475487158169896794E+00;
    weight[10-1] = 0.160743286387845749007726726449E+00;
    weight[11-1] = 0.160743286387845749007726726449E+00;
    weight[12-1] = 0.156580102647475487158169896794E+00;
    weight[13-1] = 0.148361554070916825814713013734E+00;
    weight[14-1] = 0.136300482358724184489780792989E+00;
    weight[15-1] = 0.120709227628674725099429705002E+00;
    weight[16-1] = 0.101991499699450815683781205733E+00;
    weight[17-1] = 0.806317639961196031447768461137E-01;
    weight[18-1] = 0.571818021275668260047536271732E-01;
    weight[19-1] = 0.322371231884889414916050281173E-01;
    weight[20-1] = 0.526315789473684210526315789474E-02;
  }
  else
  {
    Throw() << "\n"
     << "LOBATTO_SET - Fatal error!\n"
     << "  Illegal value of ORDER = " << order << "\n"
     << "  Legal values are between 2 and 20.\n";
  }

  return;
}

/*-----------------------------------------------------------------------*/
// Basic lagrangian chores such as interpolation
/*-----------------------------------------------------------------------*/
void ShapeLagrangeBase::Interpolate(const double fvals[], double mcoef[]) const {

  UInt nfunc = NumFunctions();

  std::copy(&fvals[0], &fvals[nfunc], mcoef);

}

void ShapeLagrangeBase::Interpolate(const fad_type fvals[], fad_type mcoef[]) const {

  UInt nfunc = NumFunctions();

  std::copy(&fvals[0], &fvals[nfunc], mcoef);

}


/*-----------------------------------------------------------------------*/
// Basic lagrangian shape function evaluation
/*-----------------------------------------------------------------------*/
template <typename Real>
Real lagrange(int i, const double *x, const Real &xbar, int n)
{
   int j;

  Real l = 1.0;
  
   for (j=0; j<n; j++) {
     if(j != i) 
       l *= (xbar-x[j])/(x[i]-x[j]);
   }
   
   return l;
}

/*-----------------------------------------------------------------------*/
// 1D continuous lagrange
/*-----------------------------------------------------------------------*/
static void build_bar_dofs(UInt q, std::vector<int> &table) {
    
  // Int dofs first
  for (UInt i = 0; i < 2; i++) {
    table.push_back(DOF_NODE); 
    table.push_back(i); // ordinal
    table.push_back(0); // index
    table.push_back(1); // polarity
  } 
  
  // And now edge dofs
  for (UInt i = 0; i < 1; i++) {
    // q -1 on each edge
    for (UInt j = 1; j < q; j++) {
      table.push_back(DOF_EDGE);
      table.push_back(i); // ordinal
      table.push_back(j-1);
      table.push_back(1); // orientation
    }
  }
}

void ShapeLagrangeBar::build_itable(UInt nfunc, UInt q, std::vector<double> &ip) {
  
  // First the nodes:
  ip.clear(); ip.resize(nfunc*1, 0);
  ip[0] = -1;
  ip[1] = 1;

  // Now q-1 integration points along the edges.
  UInt ofs = 2;

  // edge 0
  for (UInt i = 1; i < q; i++) {
    ip[ofs++] = lobatto_points[i];
  }

  ThrowRequire(ofs == nfunc*1);
}

static std::map<UInt,ShapeLagrangeBar*> &get_bar_lgmap() {
  static std::map<UInt,ShapeLagrangeBar*> lgmap;
  
  return lgmap;
}


ShapeLagrangeBar *ShapeLagrangeBar::instance(UInt _q) {
  
  std::map<UInt,ShapeLagrangeBar*> &lgMap = get_bar_lgmap();
  
  std::map<UInt,ShapeLagrangeBar*>::iterator qi =
    lgMap.find(_q);
  ShapeLagrangeBar *qp;
  if (qi == lgMap.end()) {
    qp = new ShapeLagrangeBar(_q);
    lgMap[_q] = qp;
  } else qp = qi->second;
  return qp;
}

ShapeLagrangeBar::ShapeLagrangeBar(UInt _q) 
{
  q = _q;
  lobatto_points.resize(_q+1);
  
  std::vector<double> temp_w(q+1, 0);
  
  lobatto_set(q+1, &lobatto_points[0], &temp_w[0]);

  char buf[512];
  
  std::sprintf(buf, "LagrangeBar_%02d", q);
  
  ename = std::string(buf);
  
  build_bar_dofs(q, dofs);
  
  build_itable(NumFunctions(), q, ipoints);
  
}

ShapeLagrangeBar::~ShapeLagrangeBar() {
}

UInt ShapeLagrangeBar::NumFunctions() const {
  return 2 // nodes 
  + 1*(q-1); // edges
}


template <typename Real>
void ShapeLagrangeBar::shape_eval(UInt npts, const Real pcoord[], Real results[]) const {
  
  UInt nfunc = NumFunctions();
  UInt pdim = ParametricDim();
 
  std::vector<Real> x_shape((q+1)*npts, 0);
  
  // evaluate 
  for (UInt i = 0; i < q+1; i++) {
    
    for (UInt j = 0; j < npts; j++) {
      
      x_shape[j*(q+1)+i] = lagrange(i, &lobatto_points[0], pcoord[j], q+1);
      
    }
    
  }

  // And now contract to get results
  
  // Nodes
  for (UInt n = 0; n < npts; n++) {
    
    UInt nnfunc = n*nfunc;
    
    UInt qn = (q+1)*n;
    results[nnfunc] = x_shape[qn];
    results[nnfunc+1] = x_shape[qn+q];
    
  }
  
  // Edges
  for (UInt n = 0; n < npts; n++) {
    UInt qn = (q+1)*n;
    UInt nnfunc = n*nfunc;
    
    Real *r = &x_shape[qn];

    UInt ebase = nnfunc + 2;
      
    for (UInt i = 1; i < q; i++)
        results[ebase+(i-1)] = r[i];

  }  // n
  
}

template <typename Real>
void bar_shape_grad_eval(UInt npts, const Real pcoord[], Real results[]) {
}

void ShapeLagrangeBar::shape(UInt npts, const double pcoord[], double results[]) const {
  shape_eval(npts, pcoord, results);
}

void ShapeLagrangeBar::shape(UInt npts, const fad_type pcoord[], fad_type results[]) const {
  shape_eval(npts, pcoord, results);
}

void ShapeLagrangeBar::shape_grads(UInt npts, const double pcoord[], double results[]) const {

  UInt nfunc = NumFunctions();

  std::vector<fad_type> shape_fad(nfunc);

  UInt pdim = ParametricDim();

  ThrowAssert(pdim == 1);

  std::vector<fad_type> pcoord_fad(pdim);

  for (UInt i = 0; i < npts; i++) {
    pcoord_fad[0] = pcoord[i*pdim];
    pcoord_fad[0].diff(0, 1);
    
    shape_eval(1, &pcoord_fad[0], &shape_fad[0]);

    for (UInt j = 0; j < nfunc; j++) {
      const double *diff = &(shape_fad[j].fastAccessDx(0));

      results[(i*nfunc+j)] = diff[0];
    }
  }


/*
Par::Out() << "Lag shape grads: nqpoints:" << npts << ", nfunc=" << nfunc << std::endl;
std::copy(&results[0], &results[npts*nfunc*pdim], std::ostream_iterator<double>(Par::Out(), " "));
Par::Out() << std::endl;
*/

}

void ShapeLagrangeBar::shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const {
  Throw() << "do you even need this for the fad_type??";
}


/*-----------------------------------------------------------------------*/
// Quadratic continuous lagrange
/*-----------------------------------------------------------------------*/
static void build_dofs(UInt q, std::vector<int> &table) {
    
  // Int dofs first
  for (UInt i = 0; i < 4; i++) {
    table.push_back(DOF_NODE); 
    table.push_back(i); // ordinal
    table.push_back(0); // index
    table.push_back(1); // polarity
  } 
  
  // And now edge dofs
  for (UInt i = 0; i < 4; i++) {
    // q -1 on each edge
    for (UInt j = 1; j < q; j++) {
      table.push_back(DOF_EDGE);
      table.push_back(i); // ordinal
      table.push_back(j-1);
      table.push_back(1); // orientation
    }
  }
  

  // And, finally, the interior dofs
  UInt idx = 0;
  for (UInt i = 1; i < q; i++) {
    for (UInt j = 1; j < q; j++) {
      table.push_back(DOF_ELEM);
      table.push_back(0); // no ordinal for element
      table.push_back(idx++);
      table.push_back(1); // index
    }
  }

}

void ShapeLagrangeQuad::build_itable(UInt nfunc, UInt q, std::vector<double> &ip) {
  
  // First the nodes:
  ip.clear(); ip.resize(nfunc*2, 0);
  ip[0] = -1; ip[1] = -1;
  ip[2] = 1; ip[3] = -1;
  ip[4] = 1; ip[5] = 1;
  ip[6] = -1; ip[7] = 1;

  // Now q-1 integration points along the edges.
  UInt ofs = 8;

  // edge 0
  for (UInt i = 1; i < q; i++) {
    ip[ofs++] = lobatto_points[i];
    ip[ofs++] = -1;
  }
  // edge 1
  for (UInt i = 1; i < q; i++) {
    ip[ofs++] = 1;
    ip[ofs++] = lobatto_points[i];
  }
  
  // edge 2
  for (UInt i = 1; i < q; i++) {
    ip[ofs++] = lobatto_points[q-i];
    ip[ofs++] = 1;
  }
  
  // edge 3
  for (UInt i = 1; i < q; i++) {
    ip[ofs++] = -1;
    ip[ofs++] = lobatto_points[q-i];
  }

  // And now, the beloved interior (tensor product
  for (UInt i = 1; i < q; i++) {
    for (UInt j = 1; j < q; j++) {
      ip[ofs++] = lobatto_points[j];
      ip[ofs++] = lobatto_points[i];
    }
  }
  ThrowRequire(ofs == nfunc*2);
}

static std::map<UInt,ShapeLagrangeQuad*> &get_lgmap() {
  static std::map<UInt,ShapeLagrangeQuad*> lgmap;
  
  return lgmap;
}


ShapeLagrangeQuad *ShapeLagrangeQuad::instance(UInt _q) {
  
  std::map<UInt,ShapeLagrangeQuad*> &lgMap = get_lgmap();
  
  std::map<UInt,ShapeLagrangeQuad*>::iterator qi =
    lgMap.find(_q);
  ShapeLagrangeQuad *qp;
  if (qi == lgMap.end()) {
    qp = new ShapeLagrangeQuad(_q);
    lgMap[_q] = qp;
  } else qp = qi->second;
  return qp;
}

ShapeLagrangeQuad::ShapeLagrangeQuad(UInt _q) 
{
  q = _q;
  lobatto_points.resize(_q+1);
  
  std::vector<double> temp_w(q+1, 0);
  
  lobatto_set(q+1, &lobatto_points[0], &temp_w[0]);

  char buf[512];
  
  std::sprintf(buf, "LagrangeQuad_%02d", q);
  
  ename = std::string(buf);
  
  build_dofs(q, dofs);
  
  build_itable(NumFunctions(), q, ipoints);
  
}

ShapeLagrangeQuad::~ShapeLagrangeQuad() {
}

UInt ShapeLagrangeQuad::NumFunctions() const {
  return 4 // nodes 
  + 4*(q-1) // edges
  + (q-1)*(q-1); // element
}



template <typename Real>
void ShapeLagrangeQuad::shape_eval(UInt npts, const Real pcoord[], Real results[]) const {
  
  UInt nfunc = NumFunctions();
  UInt pdim = ParametricDim();
 
  std::vector<Real> x_shape((q+1)*npts, 0);
  std::vector<Real> y_shape((q+1)*npts,0);
  
  // evaluate 
  for (UInt i = 0; i < q+1; i++) {
    
    for (UInt j = 0; j < npts; j++) {
      
      x_shape[j*(q+1)+i] = lagrange(i, &lobatto_points[0], pcoord[j*pdim], q+1);
      y_shape[j*(q+1)+i] = lagrange(i, &lobatto_points[0], pcoord[j*pdim+1], q+1);
      
    }
    
  }

  // And now contract to get results
  
  // Nodes
  for (UInt n = 0; n < npts; n++) {
    
    UInt nnfunc = n*nfunc;
    
    UInt qn = (q+1)*n;
    results[nnfunc] = x_shape[qn]*y_shape[qn];
    results[nnfunc+1] = x_shape[qn+q]*y_shape[qn];
    results[nnfunc+2] = x_shape[qn+q]*y_shape[qn+q];
    results[nnfunc+3] = x_shape[qn]*y_shape[qn+q];
    
  }
  
  // Edges
  for (UInt n = 0; n < npts; n++) {
    UInt qn = (q+1)*n;
    UInt nnfunc = n*nfunc;
    
    // The stationary shape function
    Real spt[] = { y_shape[qn],
                  x_shape[qn+q],
                  y_shape[qn+q],
                  x_shape[qn] };
    
    Real *row[] = {
        &x_shape[qn],
        &y_shape[qn],
        &x_shape[qn],
        &y_shape[qn] };
                          
    for (UInt e = 0; e < 4; e++) {
      
      UInt ebase = nnfunc + 4 + e*(q-1);
      
      Real s = spt[e];
      Real *r = row[e];
      
      if (e < 2) {
        for (UInt i = 1; i < q; i++)
        results[ebase+(i-1)] = s*r[i];
      } else {
        for (UInt i = 1; i < q; i++)
         results[ebase+(i-1)] = s*r[q-i];
      }
      
    }

  }  // n
  
  // Bubbles
  for (UInt n = 0; n < npts; n++) {
    
    UInt nnfunc = n*nfunc;
    UInt qn = (q+1)*n;
    UInt base = nnfunc + 4 + 4*(q-1);
    
    UInt of = 0;
    for (UInt i = 1; i < q; i++) {
      for (UInt j = 1; j < q; j++) {
        
        results[base+of] = x_shape[qn + j]*y_shape[qn + i];
        
        ++of;
      }
    }
    
  } // n
  
}

template <typename Real>
void shape_grad_eval(UInt npts, const Real pcoord[], Real results[]) {
}

void ShapeLagrangeQuad::shape(UInt npts, const double pcoord[], double results[]) const {
  shape_eval(npts, pcoord, results);
}

void ShapeLagrangeQuad::shape(UInt npts, const fad_type pcoord[], fad_type results[]) const {
  shape_eval(npts, pcoord, results);
}

void ShapeLagrangeQuad::shape_grads(UInt npts, const double pcoord[], double results[]) const {

  UInt nfunc = NumFunctions();

  std::vector<fad_type> shape_fad(nfunc);

  UInt pdim = ParametricDim();

  ThrowAssert(pdim == 2);

  std::vector<fad_type> pcoord_fad(pdim);

  for (UInt i = 0; i < npts; i++) {
    pcoord_fad[0] = pcoord[i*pdim];
    pcoord_fad[0].diff(0, 2);
    
    pcoord_fad[1] = pcoord[i*pdim+1];
    pcoord_fad[1].diff(1, 2);

    shape_eval(1, &pcoord_fad[0], &shape_fad[0]);

    for (UInt j = 0; j < nfunc; j++) {
      const double *diff = &(shape_fad[j].fastAccessDx(0));

      results[(i*nfunc+j)*pdim] = diff[0];
      results[(i*nfunc+j)*pdim+1] = diff[1];
    }
  }


/*
Par::Out() << "Lag shape grads: nqpoints:" << npts << ", nfunc=" << nfunc << std::endl;
std::copy(&results[0], &results[npts*nfunc*pdim], std::ostream_iterator<double>(Par::Out(), " "));
Par::Out() << std::endl;
*/

}

void ShapeLagrangeQuad::shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const {
  Throw() << "do you even need this for the fad_type??";
}

/*-----------------------------------------------------------------------*/
// Quadratic dis-continuous lagrange
/*-----------------------------------------------------------------------*/

static void build_dofs_dg(UInt q, std::vector<int> &table) {
    
  // And, finally, the interior dofs
  UInt idx = 0;
  for (UInt i = 0; i < q+1; i++) {
    for (UInt j = 0; j < q+1; j++) {
      table.push_back(DOF_ELEM);
      table.push_back(0); // no ordinal for element
      table.push_back(idx++);
      table.push_back(1); // index
    }
  }

}

void ShapeLagrangeQuadDG::build_itable(UInt nfunc, UInt q, std::vector<double> &ip) {
  
  // First the nodes:
  ip.clear(); ip.resize(nfunc*2, 0);

  // And now, the beloved interior (tensor product
  UInt ofs = 0;
  for (UInt i = 0; i < q+1; i++) {
    for (UInt j = 0; j < q+1; j++) {
      ip[ofs++] = lobatto_points[j];
      ip[ofs++] = lobatto_points[i];
    }
  }

  ThrowRequire(ofs == nfunc*2);
}

static std::map<UInt,ShapeLagrangeQuadDG*> &get_lgdgmap() {
  static std::map<UInt,ShapeLagrangeQuadDG*> lgdgmap;
  
  return lgdgmap;
}


ShapeLagrangeQuadDG *ShapeLagrangeQuadDG::instance(UInt _q) {
  
  std::map<UInt,ShapeLagrangeQuadDG*> &lgdgMap = get_lgdgmap();
  
  std::map<UInt,ShapeLagrangeQuadDG*>::iterator qi =
    lgdgMap.find(_q);
  ShapeLagrangeQuadDG *qp;
  if (qi == lgdgMap.end()) {
    qp = new ShapeLagrangeQuadDG(_q);
    lgdgMap[_q] = qp;
  } else qp = qi->second;
  return qp;
}

ShapeLagrangeQuadDG::ShapeLagrangeQuadDG(UInt _q) 
{
  q = _q;
  lobatto_points.resize(_q+1);
  
  std::vector<double> temp_w(q+1, 0);
  
  lobatto_set(q+1, &lobatto_points[0], &temp_w[0]);

  char buf[512];
  
  std::sprintf(buf, "LagrangeQuad_%02d", q);
  
  ename = std::string(buf);
  
  build_dofs_dg(q, dofs);
  
  build_itable(NumFunctions(), q, ipoints);
  
}

ShapeLagrangeQuadDG::~ShapeLagrangeQuadDG() {
}

UInt ShapeLagrangeQuadDG::NumFunctions() const {
  return (q+1)*(q+1); // element
}


template <typename Real>
void ShapeLagrangeQuadDG::shape_eval(UInt npts, const Real pcoord[], Real results[]) const {
  
  UInt nfunc = NumFunctions();
  UInt pdim = ParametricDim();
 
  std::vector<Real> x_shape((q+1)*npts, 0);
  std::vector<Real> y_shape((q+1)*npts,0);
  
  // evaluate 
  for (UInt i = 0; i < q+1; i++) {
    
    for (UInt j = 0; j < npts; j++) {
      
      x_shape[j*(q+1)+i] = lagrange(i, &lobatto_points[0], pcoord[j*pdim], q+1);
      y_shape[j*(q+1)+i] = lagrange(i, &lobatto_points[0], pcoord[j*pdim+1], q+1);
      
    }
    
  }

  // And now contract to get results
  
  // Bubbles
  for (UInt n = 0; n < npts; n++) {
    
    UInt nnfunc = n*nfunc;
    UInt qn = (q+1)*n;
    UInt base = nnfunc;
    
    UInt of = 0;
    for (UInt i = 0; i < q+1; i++) {
      for (UInt j = 0; j < q+1; j++) {
        
        results[base+of] = x_shape[qn + j]*y_shape[qn + i];
        
        ++of;
      }
    }
    
  } // n
  
}

void ShapeLagrangeQuadDG::shape(UInt npts, const double pcoord[], double results[]) const {
  shape_eval(npts, pcoord, results);
}

void ShapeLagrangeQuadDG::shape(UInt npts, const fad_type pcoord[], fad_type results[]) const {
  shape_eval(npts, pcoord, results);
}

void ShapeLagrangeQuadDG::shape_grads(UInt npts, const double pcoord[], double results[]) const {
Throw() << "DG shape grad not yet implemented";
}

void ShapeLagrangeQuadDG::shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const {
Throw() << "DG shape grad not yet implemented";
}

template void ShapeLagrangeQuad::shape_eval(UInt npts, const double pcoord[], double results[]) const; 
template void ShapeLagrangeQuad::shape_eval(UInt npts, const fad_type pcoord[], fad_type results[]) const; 

template void ShapeLagrangeQuadDG::shape_eval(UInt npts, const double pcoord[], double results[]) const; 
template void ShapeLagrangeQuadDG::shape_eval(UInt npts, const fad_type pcoord[], fad_type results[]) const; 

} // namespace
