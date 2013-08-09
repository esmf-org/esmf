<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="TopConfig">
    <HTML>
      <HEAD>
        <TITLE>
          <xsl:value-of select="TopFname"/>
        </TITLE>
      </HEAD>
      <BODY>
        Test Case: <xsl:value-of select="TopFname"/><BR/>
        Config File Path: <xsl:value-of select="ConfigPath"/><BR/>
        Timestamp: <xsl:value-of select="Timestamp"/><BR/>
        Test Class: <xsl:value-of select="TestClass"/><BR/>
        <xsl:apply-templates/>
      </BODY>
    </HTML>
  </xsl:template>

  <xsl:template match="ProbDescString">
    Operation: <xsl:value-of select="Process"/><BR/>
    Descriptor: <xsl:value-of select="PDS"/><BR/>
    <P><TABLE BORDER="2" CELLPADDING="4">
        <TR>
          <TH BGCOLOR="#b0b0b0"/>
          <TH COLSPAN="7">Distribution</TH>
          <TH COLSPAN="2" BGCOLOR="#b0b0b0" WIDTH="5%"/>
          <TH COLSPAN="7">Grid</TH>
        </TR>
        <TR>
           <TH BGCOLOR="#b0b0b0"/>
           <TH COLSPAN="3">Source</TH>
           <TH COLSPAN="1" BGCOLOR="#b0b0b0"/>
           <TH COLSPAN="3">Dest</TH>
           <TH COLSPAN="2" BGCOLOR="#b0b0b0"/>
           <TH COLSPAN="3">Source</TH>
           <TH COLSPAN="1" BGCOLOR="#b0b0b0"/>
           <TH COLSPAN="3">Dest</TH>
        </TR>
        <TR>
          <TD BGCOLOR="#b0b0b0"/>
          <TD>X</TD>
          <TD>Y</TD>
          <TD>Z</TD>
          <TD BGCOLOR="#b0b0b0"/>
          <TD>X</TD>
          <TD>Y</TD>
          <TD>Z</TD>
          <TD COLSPAN="2" BGCOLOR="#b0b0b0"/>
          <TD>X</TD>
          <TD>Y</TD>
          <TD>Z</TD>
          <TD BGCOLOR="#b0b0b0"/>
          <TD>X</TD>
          <TD>Y</TD>
          <TD>Z</TD>
        </TR>
        <xsl:apply-templates/>
      </TABLE>
    </P>
  </xsl:template>

  <xsl:template match="DistGridFile/Row">
    <TR VALIGN="TOP">
      <TD>
      Dimension <BR/>
      Low Coord <BR/>
      High Coord <BR/>
      Grid Type <BR/>
      UOM <BR/>
      </TD>
      <TD>
        <xsl:value-of select="SourceDistSize1"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceDistSize2"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceDistSize3"/>
      </TD>
      <TD BGCOLOR="#b0b0b0"/>
      <TD>
        <xsl:value-of select="DestDistSize1"/>
      </TD>
      <TD>
        <xsl:value-of select="DestDistSize2"/>
      </TD>
      <TD>
        <xsl:value-of select="DestDistSize3"/>
      </TD>
      <TD COLSPAN="2" BGCOLOR="#b0b0b0"/>
      <TD>
        <xsl:value-of select="SourceGridSize1"/> <BR/>
        <xsl:value-of select="SourceGridCoordLow1"/> <BR/>
        <xsl:value-of select="SourceGridCoordHigh1"/> <BR/>
        <xsl:value-of select="SourceGridType1"/> <BR/>
        <xsl:value-of select="SourceGridUnits1"/> <BR/>
      </TD>
      <TD>
        <xsl:value-of select="SourceGridSize2"/> <BR/>
        <xsl:value-of select="SourceGridCoordLow2"/> <BR/>
        <xsl:value-of select="SourceGridCoordHigh2"/> <BR/>
        <xsl:value-of select="SourceGridType2"/> <BR/>
        <xsl:value-of select="SourceGridUnits2"/> <BR/>
      </TD>
      <TD>
        <xsl:value-of select="SourceGridSize3"/> <BR/>
        <xsl:value-of select="SourceGridCoordLow3"/> <BR/>
        <xsl:value-of select="SourceGridCoordHigh3"/> <BR/>
        <xsl:value-of select="SourceGridType3"/> <BR/>
        <xsl:value-of select="SourceGridUnits3"/> <BR/>
      </TD>
      <TD BGCOLOR="#b0b0b0"/>
      <TD>
        <xsl:value-of select="DestGridSize1"/> <BR/>
        <xsl:value-of select="DestGridCoordLow1"/> <BR/>
        <xsl:value-of select="DestGridCoordHigh1"/> <BR/>
        <xsl:value-of select="DestGridType1"/> <BR/>
        <xsl:value-of select="DestGridUnits1"/> <BR/>
      </TD>
      <TD>
        <xsl:value-of select="DestGridSize2"/> <BR/>
        <xsl:value-of select="DestGridCoordLow2"/> <BR/>
        <xsl:value-of select="DestGridCoordHigh2"/> <BR/>
        <xsl:value-of select="DestGridType2"/> <BR/>
        <xsl:value-of select="DestGridUnits2"/> <BR/>
      </TD>
      <TD>
        <xsl:value-of select="DestGridSize3"/> <BR/>
        <xsl:value-of select="DestGridCoordLow3"/> <BR/>
        <xsl:value-of select="DestGridCoordHigh3"/> <BR/>
        <xsl:value-of select="DestGridType3"/> <BR/>
        <xsl:value-of select="DestGridUnits3"/> <BR/>
      </TD>
    </TR>
  </xsl:template>

  <xsl:template match="text()|@*"></xsl:template>
</xsl:stylesheet>
