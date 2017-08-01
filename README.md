# enp.script
R Script: Laakso and Taagepera's Effective Number of Parties
=====================================

Description
------------------
This function provides the Laakso and Taagepera's Effective Number of Parties (ENP) for both seats and votes. It stores the ENP in a data frame, and it may print the results into an output file.

Availability
------------------
The script can be called directly from the repository:
<pre><code>source('https://raw.github.com/santiago-alles/enp.script/master/enp.v1.4.r')</code></pre>

Usage
------------------

<pre><code>enp.FUN( x,
          votes = "votes", seats = "seats",
          year = "year", chamber = "chamber", district = "district",
          path = getwd(),
          enp_v = T, enp_s = T,
          save = F )</code></pre>

Arguments
------------------

<code>x</code> Data frame containing the input data set.

<code>votes</code> Vector in <code>x</code> frame with the absolute number of party votes. Non-valid votes should be dropped from the frame before running the estimation.

<code>seats</code> Vector in <code>x</code> frame with the number of allocated seats.

<code>year</code> Vector in <code>x</code> frame with the election year.

<code>chamber</code> Vector in <code>x</code> frame with the legislative chamber.

<code>district</code> Vector in <code>x</code> frame with the district label.

<code>path</code> It sets the working directory where results will be stored if <code>save == T</code>. If not defined, it will use the filepath representing the current working directory.

<code>enp_v</code> If TRUE, it estimates the electoral ENP using votes.

<code>enp_s</code> If TRUE, it estimates the legislative ENP using seats.

<code>save</code> If FALSE, it only keeps a data frame in memory with the results. If TRUE, it additionally stores a file with the results in a new, self-generated folder (see: <code>path</code> above).

Input Data
------------------

The dataset has to include the following variables:

<code>Year</code> It may be either a single year or multiple years. The variable has to be numeric.

<code>Chamber</code> It may be a single chamber or more than one. The variable may be coded as a string (e.g., 'House', 'Senate'), or as numeric.

<code>District</code> The ENP will be aggregated at this level. It may be coded as a string (e.g., 'Ayacucho', 'Cajamarca', 'Callao'), or as numeric.

<code>Votes</code> Number of votes for each party in the district, in a given election year. The dataset do not need to be symmetrical: if a given party did not run in every district, it can be ommitted in those districts. Non-valid votes should be dropped.

<code>Seats</code> Number of seats obtained by each party in the district, in a given election year. When a party did not get any seat in a district, the cell might be either empty (NA) or coded as '0'.

Should data include blank and null votes?

No. Blank and null votes have to be excluded. Otherwise, the script will count them as a party, and the resulting ENP estimation will be inaccurate.

What will be the output?

The ENP will be stored in a data frame in memory. If "save" is TRUE, a spreadsheet in a .csv file (which can be easily open using Microsoft Excel) will be stored in the working directory, unless another location is defined in the <code>path</code>.

Required packages 
------------------

<pre><code>doBy</pre></code>
<pre><code>stringr</pre></code>

If not installed, the function will try to install them.
