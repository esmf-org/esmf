<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="TopConfig">
\subsection{Test Suite: <xsl:value-of select="TopFname"/>}
Test Class: <xsl:value-of select="TestClass"/> \\
Timestamp: <xsl:value-of select="Timestamp"/> \\ \\
<xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="ProbDescString">
Operation: <xsl:value-of select="Process"/> \\
Descriptor: <xsl:value-of select="PDS"/> \\
\tablefirsthead {
\hline
&amp;\multicolumn{3}{|c|}{Source}&amp;\multicolumn{3}{|c|}{Destination} \\ \hline
&amp;X&amp;Y&amp;Z&amp;X&amp;Y&amp;Z \\ \hline
}
\tablehead {}
\tabletail {\hline}
\tablelasttail {}
<xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="DistGridFile/Row">
\begin{supertabular}{|p{0.75in}|p{1.25in}|p{1.25in}|p{1.25in}|
    p{1.25in}|p{1.25in}|p{1.25in}|}
Dist Dim &amp; 
<xsl:value-of select="SourceDistSize1"/> &amp; 
<xsl:value-of select="SourceDistSize2"/> &amp; 
<xsl:value-of select="SourceDistSize3"/> &amp;
<xsl:value-of select="DestDistSize1"/> &amp; 
<xsl:value-of select="DestDistSize2"/> &amp;
<xsl:value-of select="DestDistSize3"/> \\ \hline
Grid Dim &amp; 
<xsl:value-of select="SourceGridSize1"/> &amp; 
<xsl:value-of select="SourceGridSize2"/> &amp;
<xsl:value-of select="SourceGridSize3"/> &amp;
<xsl:value-of select="DestGridSize1"/> &amp; 
<xsl:value-of select="DestGridSize2"/> &amp; 
<xsl:value-of select="DestGridSize3"/> \\ \hline
Low Coord &amp; 
<xsl:value-of select="SourceGridCoordLow1"/> &amp; 
<xsl:value-of select="SourceGridCoordLow2"/> &amp; 
<xsl:value-of select="SourceGridCoordLow3"/> &amp;
<xsl:value-of select="DestGridCoordLow1"/> &amp; 
<xsl:value-of select="DestGridCoordLow2"/> &amp; 
<xsl:value-of select="DestGridCoordLow3"/> \\ \hline
High Coord &amp; 
<xsl:value-of select="SourceGridCoordHigh1"/> &amp; 
<xsl:value-of select="SourceGridCoordHigh2"/> &amp; 
<xsl:value-of select="SourceGridCoordHigh3"/> &amp;
<xsl:value-of select="DestGridCoordHigh1"/> &amp; 
<xsl:value-of select="DestGridCoordHigh2"/> &amp; 
<xsl:value-of select="DestGridCoordHigh3"/> \\ \hline
Grid Type &amp; 
<xsl:value-of select="SourceGridType1"/> &amp; 
<xsl:value-of select="SourceGridType2"/> &amp; 
<xsl:value-of select="SourceGridType3"/> &amp;
<xsl:value-of select="DestGridType1"/> &amp; 
<xsl:value-of select="DestGridType2"/> &amp; 
<xsl:value-of select="DestGridType3"/> \\ \hline
UOM &amp; 
<xsl:value-of select="SourceGridUnits1"/> &amp; 
<xsl:value-of select="SourceGridUnits2"/> &amp; 
<xsl:value-of select="SourceGridUnits3"/> &amp;
<xsl:value-of select="DestGridUnits1"/> &amp; 
<xsl:value-of select="DestGridUnits2"/> &amp; 
<xsl:value-of select="DestGridUnits3"/> \\ \hline
\end{supertabular}
\vspace{0.25in}
  </xsl:template>

  <xsl:template match="text()|@*"></xsl:template>
</xsl:stylesheet>
