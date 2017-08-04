
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	version="1.0">
	<xsl:output method="html" indent="yes" omit-xml-declaration="yes" />

	<xsl:template match="analysisProject">
		<html>
			<head>
				<style>

					tr:nth-child(odd) { background: #E0E0E0; }
					tr:nth-child(even)
					{ background: #D0D0D0; }

				</style>
				<title>
					ICode <xsl:value-of select="@analysisProjectName" />
				</title>
			</head>
			<body>
				<h1>ICode Analysis information</h1>
				<ul>
					<li>
						Project name :
						<xsl:value-of select="@analysisProjectName" />
					</li>
					<li>
						Version :
						<xsl:value-of select="@analysisProjectVersion" />
					</li>
					<li>
						Configuration ID :
						<xsl:value-of select="analysisInformations/@analysisConfigurationId" />
					</li>
					<li>
						Date :
						<xsl:value-of select="analysisInformations/@analysisDate" />
					</li>
					<li>
						Author :
						<xsl:value-of select="analysisInformations/@author" />
					</li>




					<analysisInformations analysisConfigurationId="analysis1"
						analysisDate="2017-08-03" author="i-Code CNES Analyzer" />

				</ul>

				<xsl:for-each select="analysisFile">
					<xsl:variable name="fn" select="@fileName" />

					<h1 align="center">
						Analysis of
						<xsl:value-of select="$fn" />
					</h1>
					<table>
						<tr>
							<th>Analysis Rule ID</th>
							<th align="center">Result ID</th>
							<th>Line</th>
							<th>Type Place</th>
							<th>Name Place</th>
							<th>Message</th>

						</tr>
						<xsl:apply-templates select="../analysisRule[result/@fileName=$fn]" />


					</table>


				</xsl:for-each>
			</body>

		</html>

	</xsl:template>

	<xsl:template match="analysisRule">

		<tr>
			<td>
				<xsl:value-of select="@analysisRuleId" />
			</td>
			<xsl:apply-templates />
		</tr>


	</xsl:template>

	<xsl:template match="result">
		<td align="center">
			<xsl:value-of select="@resultId" />
		</td>
		<td align="center">
			<xsl:value-of select="@resultLine" />
		</td>
		<td align="center">
			<xsl:value-of select="@resultTypePlace" />
		</td>
		<td align="center">
			<xsl:value-of select="@resultNamePlace" />
		</td>

		<td>
			<xsl:value-of select="resultMessage" />
		</td>


		<!-- <result resultId="213" fileName="/Users/olivier/tmp/ccm_type.f90" 
			resultLine="409" resultTypePlace="method" resultNamePlace="subroutine writeccm"> 
			<resultMessage>The code is not indented.</resultMessage> </result> -->



	</xsl:template>



</xsl:stylesheet>