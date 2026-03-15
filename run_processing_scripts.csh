#!/bin/csh
#
# run_processing_scripts.csh
#
# Process through all the various combinations of start/end years,
# minimum record lengths, durations, and seasonal aggregations

# set echo

# for testing/debugging:
# set testing

testing:
if ($?testing) then    # set your own values here
  echo "Running with specified values in testing mode"
  set run_echo = "echo "
  # set minimum and maximum trend window lengths
  set dt_s = 10
  # set dt_e = 56
  set record_lengths = ( 0 ) # others to try: 20, 25, 30
  set start_years = ( 1973 )  # can set to one or multiple start years e.g. ( 1951 1961 1971 1976 1981 1986)
  set end_years = ( 2009 )
  set durations = ( 1 )   # 1 3 6 12 24
  set aggregations = ( Annual )  # Annual Seasonal
  set min_proportions = ( 67 ) # can test differing values for the minimum proportion of valid years
  set sig_level = 0.05  # can set this to stricter/looser values of p-value
  set default_start = 1960
  set final_year = 2015
else
  set run_echo = ""
  set dt_s = 10
  set record_lengths = (0)  # default 0 to create stats for all trend lengths
  set start_years = (1960)  # default 1960
  set end_years = (2015)    # default 2015
  set durations = (1 3 6 12 24)
  set aggregations = (Annual Seasonal)
  set min_proportions = (67)  # others: 50 67 80
  set sig_level = 0.05
  set default_start = 1960
  set final_year = 2015
endif


# Parse command line options
if ($#argv < 1) then
  #there must be at least one command line argument
  echo ""
  echo "A command line argument required."
  set fault
  goto fault
else
  if (! $?testing) set args = ($argv)
  # echo "n_args: $#argv"
  # echo $args
endif

set n = 1
while ($n <= $#args)
  # echo $args $n
  # echo $args[$n]
  set arg = $args[$n]
  # echo "arg: $arg"
  # also must have a leading dash
  if (`echo $arg | grep '-' | wc -l` != 1) then
    set fault
    goto fault
  endif


  switch ($arg)
    case --fig*:
      set script_option = figures
      breaksw
    case -f:
      set script_option = figures
      breaksw
    case --tab*:
      set script_option = tables
      breaksw
    case -t:
      set script_option = tables
      breaksw
    case --map*:
      set script_option = choropleths
      breaksw
    case -m:
      set script_option = choropleths
      breaksw
    case --all:
      set script_option = all
      breaksw
    case -a:
      set script_option = all
      breaksw
    case --start:
      @ n ++
      set first_start = $args[$n]
      breaksw
    case -s:
      @ n ++
      set first_start = $args[$n]
      breaksw
    case --end:
      @ n ++
      set last_year = $args[$n]
      breaksw
    case -e:
      @ n ++
      set last_year = $args[$n]
      breaksw
    case --period_min:
      @ n ++
      set dt_s = $args[$n]
      breaksw
    case -p:
      @ n ++
      set dt_s = $args[$n]
      breaksw
    case --testing:
      echo "Running with specified values in testing mode"
      # remove '--testing' from $arg
      set args = (`echo $args | sed 's/--testing//'`)
      # go back to top and re-load vars that can be over-written by command line options
      set testing
      goto testing
      breaksw
    default:
      echo "Unknown option: $argv"
      set fault
      goto fault
      breaksw
  endsw

  @ n ++

end

fault:
if ($?fault) then

  echo ""
  echo " Options: -f|--figures, -t|--tables, -m|--maps, [-s|--start <start_year>], [-e|--end <end_year>], [-p|--period_min <min_period_length>], [--testing]"
  echo ""
  exit 1

endif

if ($script_option == choropleths) then
  set desc_text = "choropleth maps"
else if ($script_option == all) then
  set desc_text = "all outputs - figures, tables and choropleth maps"
else 
  set desc_text = $script_option
endif

if ($?dt_s) then
  set desc_text = "${desc_text} for minimum ${dt_s} year windows"
endif

if ($?first_start) then
  set desc_text = "${desc_text} starting from ${first_start}"
else
  set first_start = $default_start
endif

if ($?last_year) then
  if ($last_year > $final_year) then
    echo ""
    echo "Last year command line option cannot be greater than final year; setting last_year to final_year ($final_year)"
    echo ""
    set last_year = $final_year
  else
    set final_year = $last_year
  endif
  set desc_text = "${desc_text} through to ${last_year}"
else
  set last_year = $final_year
endif

echo "Script set to produce $desc_text"
# exit 

# calculate dt_e - longest possible period length for given start and end years
@ dt_e = $last_year - $first_start + 1

if ($dt_e < $dt_s) then
  echo ""
  echo "Start year and final year too close to meet minimum period length ($dt_s years); exiting"
  echo ""
  exit 1
endif

foreach dt ( `seq $dt_s $dt_e` )

  @ last_start = $final_year - $dt + 1
  # this gives the last possible start year:
  # e.g. if final year is 2015 and dt is 10 (the defaults), then last_start is 2006 (10 individual years of block maxima)

  echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  echo "                 Starting ${dt} year windows"
  echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  echo ""


  foreach record_length ($record_lengths)
    foreach start_year ( `seq $first_start $last_start` )
      @ end_year = $start_year + $dt - 1
      if ($end_year > $final_year) continue
      foreach duration ($durations)
        foreach aggregation ($aggregations)
          foreach min_proportion ($min_proportions)
            
            # echo to output what is going to run:
            if (! $?testing) then
              echo ""
              echo ""
              echo "################################################"
              echo \
              Rscript processing_and_plotting.R \
                -d $duration \
                -p $aggregation \
                -r $record_length \
                -f $start_year \
                -l $end_year \
                -x $min_proportion \
                -s $sig_level \
                -o $script_option
              echo "################################################"
              echo ""
              echo ""
            endif

            # execute script:
            if ($?testing) then
              echo "TESTING ONLY - NOT EXECUTED"
            else
              echo "running script:"
            endif
            
            $run_echo \
            Rscript processing_and_plotting.R \
              -d $duration \
              -p $aggregation \
              -r $record_length \
              -f $start_year \
              -l $end_year \
              -x $min_proportion \
              -s $sig_level \
              -o $script_option

            echo ""
            echo "run complete"
            echo "################################################"
            echo ""

            # echo to a progress log file what was just run:
            if (! $?testing) then
              echo "echoing the task to processing_progress.log"
              echo "Rscript processing_and_plotting.R -d $duration -p $aggregation -r $record_length -f $start_year -l $end_year -x $min_proportion -s $sig_level -o $script_option " >> processing_progress.log
            endif

          end
        end
      end
    end
  end

end # dt

exit
