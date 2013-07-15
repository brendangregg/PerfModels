#!/bin/ksh
#
# waterfall.sh		Run a series of dist.r plots and create
#			composite waterfall plots
#
# requires: ImageMagick (convert), R.
#
# 01-Jun-2013	Brendan Gregg	Created this.

#
# Parameters
#

# synthetic
dists="0 1 2"
dists="$dists 100 101 102 103 110 111 112 120 130 131"
dists="$dists 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155"
dists="$dists 200 201 202 203 204 205 206 210 211 212 213"
dists="$dists 300 301 302 303 304 305 306 307"
dists="$dists 400 401 402 403"
dists="$dists 500 501 502 503"

# plot type
# trail: density=2 rug=1
# trailfill: density=1 rug=1 same=1
# dotfill: density=2 rug=1 same=1
# dash: density=2 rug=0
# line: density=1 rug=0
# rugfill: density=1 rug=1
# hist: density=0 rug=0
# histrug: density=0 rug=1
# density types: 0 = hist, 1 = line, 2 = frequency trail
density=1
rug=1
same=1

# input
runtype=1		# 1 = synth, 2 = random, 3 = infiles
infiles='../dilt03/*'
dist=601
trimlist="2" 		# 0 = none, 1 = sd, 2 = iqr, 3 = maxtrim
maxtrim=1000000
weight=0

# execution
outdir=working
maxpng=35
makepng=1
makecomposites=1
parallel=5
stride=1
N=5000

# layout
yoffset=55
xoffset=0
ypad=10
xpad=40
width=2300
pngwidth=2400
pngheight=220
lwd=2

#
# Combinations
#

# modal vertical:
#density=2; rug=1; same=0; trimlist=1; weight=0
#yoffset=50; xoffset=0; pngheight=220; lwd=8

# 20ms vertical compact:
#density=2; rug=1; same=0; trimlist=2; maxtrim=20000; weight=0
#yoffset=20; xoffset=0; pngheight=220; lwd=8

# 100ms staggered compact:
#density=2; rug=1; same=0; trimlist=2; maxtrim=100000; weight=0
#yoffset=20; xoffset=10; pngheight=220; lwd=8

# 100ms staggered compact filled - datacenter outliers:
#density=1; rug=1; same=0; trimlist=2; maxtrim=100000; weight=0
#yoffset=20; xoffset=10; pngheight=220; lwd=8

# 20ms diagonal:
#density=2; rug=1; same=0; trimlist=2; maxtrim=20000; weight=0
#yoffset=30; xoffset=30; pngheight=220; lwd=8

# outlier detection
#density=1; rug=1; same=1; trimlist=0; weight=0
#yoffset=55; xoffset=0; pngheight=220; lwd=8; N=50000

# modal colored
#density=1; rug=1; same=1; trimlist=0; weight=0
#yoffset=55; xoffset=0; pngheight=220; lwd=2; N=10000	# top yoffset=205
#incl. white line after polygon

mkdir -p $outdir
cd $outdir
echo output directory: $outdir

if (( makepng )); then
	rm dist_*png
	rm max_*png
	rm maxsigma_*png
	rm bimodalcf_*png
	rm diptest_*png
	rm cov_*png
	rm ydiff_*png
fi

# onedistpng
#
# environment: dist trim maxtrim pfile density weight infile maxtrim
#	pngwidth pngheight lwd
#
function onedistpng {
	echo 'source("../dist.r")' | \
	    TYPE=$dist TRIM=$trim OUTFILE=$pfile PNG=1 LABELS=0 N=$N \
	    DENSITY=$density LWD=$lwd RUG=$rug FILL=1 TRANS=1 WEIGHT=$weight \
	    RANDOM=0 INFILE=$infile MAXTRIM=$maxtrim SYMLINK=1 \
	    PNGWIDTH=$pngwidth PNGHEIGHT=$pngheight \
	    R --no-save 2>/dev/null | \
	    grep DATA | sed 's/[^ ]* //'

	# was +88
	convert $pfile -crop ${width}x$((pngheight - pngheight/24 - 1))+50+1 \
	    $pfile

	# same color
	if (( same )); then convert $pfile -negate $pfile; fi
}

# make synthetic dist pngs
function makesynth {
	for trim in $trimlist; do
		i=1; j=1
		for dist in $dists; do
			if (( i++ > maxpng )); then wait; continue; fi
			if (( $trim )); then
				pfile=`printf "dist_%03dt.png" $dist`
			else
				pfile=`printf "dist_%03df.png" $dist`
			fi
			onedistpng &
			if (( j++ >= $parallel )); then j=1; wait; fi
		done
		wait
	done
}

# make random dist pngs
function makerandom {
	for trim in $trimlist; do
		i=1; j=1
		while (( i < maxpng )); do
			if (( $trim )); then
				pfile=`printf "dist_%03dt.png" $i`
			else
				pfile=`printf "dist_%03df.png" $i`
			fi
			(( dist = 1030 + i ))
			onedistpng &
			if (( j++ >= $parallel )); then j=1; wait; fi
			if (( i++ >= maxpng )); then wait; continue; fi
		done
		wait
	done
}

# make actual dist pngs
function makereal {
	for trim in $trimlist; do
		i=1; j=1
		for infile in $infiles; do
			if (( i++ > maxpng )); then wait; continue; fi
			if (( $trim )); then
				pfile=dist_${infile##*/}t.png
			else
				pfile=dist_${infile##*/}f.png
			fi
			onedistpng &
			if (( j++ >= $parallel )); then j=1; wait; fi
		done
		wait
	done
}

# main
if (( makepng )); then
	(( runtype == 1 )) && makesynth
	(( runtype == 2 )) && makerandom
	(( runtype == 3 )) && makereal
fi
if (( !makecomposites )); then exit; fi

# makecomposite
#
# environment: ypad xpad yoffset xoffset
# input: name
#
function makecomposite {
	name=$1
	(( maxy = pngheight + ypad * 2 ))
	(( maxx = width + xpad * 2 - xoffset ))
	i=1; j=1; y=$ypad; files=

	for f in ${name}_*png; do
		if (( j++ >= stride )); then j=1; else continue; fi
		if (( i++ > maxpng )); then break; fi

		(( maxy += yoffset ))
		(( maxx += xoffset ))
		files="$files $f"
	done

	(( x = maxx - width - xpad ))
	dest=waterfall_${name}.png
	im=""

	for f in $files; do
		if [ -e $f ]; then
			im="$im $f -geometry +$x+$y -composite"
		fi
		(( y += yoffset ))
		(( x -= xoffset ))
	done

	echo making composite $dest
	convert -size ${maxx}x$maxy canvas:transparent $im $dest
}

# make composites
for trim in $trimlist; do eval trim$trim=1; done
(( trim1 || trim2 )) && makecomposite bimodalcf &
(( trim1 || trim2 )) && makecomposite diptest &
(makecomposite max) &
(makecomposite maxsigma) &
(makecomposite cov) &
(makecomposite ydiff) &
wait

function negate {
	file=$1
	white=white$2
	black=black$2
	gray=gray$2
	whitefill=whitefill$2
	blackfill=blackfill$2
	whitefile=${file%.png}_$white.png
	blackfile=${file%.png}_$black.png
	grayfile=${file%.png}_$gray.png
	whitefillfile=${file%.png}_$whitefill.png
	blackfillfile=${file%.png}_$blackfill.png
	if (( same )); then
		convert $file -background white -alpha remove -alpha off \
		    $whitefillfile
		convert $whitefillfile -negate $blackfillfile
		return
	fi
	convert $file -background white -alpha remove -alpha off $whitefile
	convert $whitefile -negate $blackfile
	convert $file -background '#909090' -alpha remove -alpha off $grayfile
}

function rugnegate {
	file=$1
	white=white$2
	black=black$2
	whitefill=whitefill$2
	blackfill=blackfill$2
	nfile=${file%.png}_negate.png
	whitefile=${file%.png}_$white.png
	blackfile=${file%.png}_$black.png
	whitefillfile=${file%.png}_$whitefill.png
	blackfillfile=${file%.png}_$blackfill.png
	if (( same )); then
		convert $file -background white -alpha remove -alpha off \
		    $whitefillfile
		convert $whitefillfile -negate $blackfillfile
		return
	fi
	convert $file -negate $nfile
	convert $nfile -background white -alpha remove -alpha off $whitefile
	convert $whitefile -negate $blackfile
}

# negations
echo making negations
if (( rug && density < 2 )); then
	nfunc=rugnegate; name=rugfill
elif (( density == 2 )); then
	nfunc=negate; name=dot
else
	nfunc=negate; name=line
fi
$nfunc waterfall_max.png $name &
$nfunc waterfall_maxsigma.png $name &
(( trmi1 || trim2 )) && $nfunc waterfall_bimodalcf.png $name &
(( trmi1 || trim2 )) && $nfunc waterfall_diptest.png $name &
$nfunc waterfall_cov.png $name &
$nfunc waterfall_ydiff.png $name &
wait
