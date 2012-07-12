<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="TopConfig">
\subsection{Test Suite: <xsl:value-of select="TopFname"/>}
Test Class: <xsl:value-of select="TestClass"/> \\
Timestamp: <xsl:value-of select="Timestamp"/> \\
<xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="ProbDescString">
Operation: <xsl:value-of select="Process"/> \\
Descriptor: <xsl:value-of select="PDS"/> \\ \\
\tablefirsthead {
  \hline
  \multicolumn{6}{|c|}{Distribution}&amp;\multicolumn{6}{|c|}{Array Bounds} \\ \hline
  \multicolumn{3}{|c|}{Source}&amp;\multicolumn{3}{|c|}{Dest} &amp;
  \multicolumn{3}{|c|}{Source}&amp;\multicolumn{3}{|c|}{Dest} \\ \hline
  X&amp;Y&amp;Z&amp;X&amp;Y&amp;Z&amp;X&amp;Y&amp;Z&amp;X&amp;Y&amp;Z \\ \hline
}
\tablehead {}
\tabletail {\hline}
\tablelasttail {}
\begin{longtable}{|p{0.375in}|p{0.375in}|p{0.375in}|
  p{0.375in}|p{0.375in}|p{0.375in}|
  p{0.375in}|p{0.375in}|p{0.375in}|
  p{0.375in}|p{0.375in}|p{0.375in}|}
<xsl:apply-templates/>
\end{longtable}
\\ \\
  </xsl:template>

  <xsl:template match="DistGridFile/Row">
<xsl:value-of select="SourceDistSize1"/> &amp; 
<xsl:value-of select="SourceDistSize2"/> &amp; 
<xsl:value-of select="SourceDistSize3"/> &amp;
<xsl:value-of select="DestDistSize1"/> &amp; 
<xsl:value-of select="DestDistSize2"/> &amp;
<xsl:value-of select="DestDistSize3"/> &amp;
<xsl:value-of select="SourceGridSize1"/> &amp; 
<xsl:value-of select="SourceGridSize2"/> &amp;
<xsl:value-of select="SourceGridSize3"/> &amp;
<xsl:value-of select="DestGridSize1"/> &amp; 
<xsl:value-of select="DestGridSize2"/> &amp; 
<xsl:value-of select="DestGridSize3"/> \\ \hline
  </xsl:template>

  <xsl:template match="text()|@*"></xsl:template>
</xsl:stylesheet>
