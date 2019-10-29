import sys
import os
import re
from collections import defaultdict, OrderedDict
import math

import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

if len(sys.argv) >= 2:
    PATH = sys.argv[1]
else:
    PATH = "20151203T9999-combined"

FILE_NAME = re.compile(r"(?P<variations>(alternatives-parallel|original)*)-t(?P<t>\d+)-i(?P<i>\d+)\.txt")

PROFILE_LINE = re.compile(r"""\s*:(?P<id>\S*)\s+(?P<n_calls>\d+)\s+(?P<min>\S+)\s+(?P<max>\S+)\s+(?P<mad>\S+)\s+(?P<mean>\S+)\s+(?P<time_percent>\S+)\s+(?P<time>\S+)\n?""")
FILE_CONTENTS = re.compile(r"""Random seed                = 1
Number of vars             = (?P<v>\d+)
Number of records          = (?P<r>\d+)
Max num parents            = (?P<n>\d+)
\% chance of parent         = [\d]+
Insert penalty             = [\d.]+
Max num edge learned / var = [-\d.]+
Operation quality factor   = [\d.]+
Variations                 = (?P<variations>.*)
Profiling\?                 = (?P<profiling_enabled>true|false)
Generating data...
Elapsed time: (?P<generate_data>[\d.]+) msecs
done.
Generating adtree...
Elapsed time: (?P<generate_adtree>[\d.]+) msecs
done.
Learning structure...
(tasks created.*\n?)+
Elapsed time: (?P<create_tasks_time>[\d.]+) msecs
((task processed|new task).*\n?)+
Elapsed time: (?P<process_tasks_time>[\d.]+) msecs
Elapsed time: (?P<learn_time>[\d.]+) msecs
done.
Learn score  = [\d.-]+
Actual score = [\d.-]+
(\d+-.+-\d+ \d+:\d+:\d+ .* TRACE \[bayes.main\] - Profiling: :bayes.main/all
                                           Id      nCalls       Min        Max       MAD      Mean   Time% Time
(?P<profiling>(\s*:(?P<id>\S*)\s+(?P<n_calls>\d+)\s+(?P<min>\S+)\s+(?P<max>\S+)\s+(?P<mad>\S+)\s+(?P<mean>\S+)\s+(?P<time_percent>\S+)\s+(?P<time>\S+)\n?)+)
                                   Clock Time                                                          \d+ (?P<clock_time>.+)
                               Accounted Time                                                          \d+ (?P<accounted_time>.+)

)?Elapsed time: (?P<elapsed_time>[\d\.]+) msecs""") #, flags=re.DOTALL)

def int_or_none(x):
    if x is None:
        return None
    else:
        return int(x)

DURATION = re.compile(r"([\d.]+)(s|ms|μs|ns)")

def parse_duration(d):
    """8.1s -> 8100
    150.9ms -> 150.9
    78.2μs  ->   0.0782
     2.0ns  ->   0.000002"""
    m = DURATION.match(d)
    number = float(m.group(1))
    unit = m.group(2)
    if unit == "s":
        factor = 1e3
    elif unit == "ms":
        factor = 1
    elif unit == "μs":
        factor = 1e-3
    elif unit == "ns":
        factor = 1e-6
    else:
        raise ValueError("Unknown duration unit", unit)
    return number * factor

def parse_files():
    results = []

    for name in os.listdir(PATH):
        nmatch = FILE_NAME.match(name)
        if nmatch is None:
            continue

        try:
            variations = nmatch.group("variations")
        except IndexError:
            variations = ""
        #pars = nmatch.group("pars")
        t = int(nmatch.group("t"))
        i = int(nmatch.group("i"))

        alternatives_parallel = "alternatives-parallel" in variations

        contents = open(os.path.join(PATH, name)).read()
        cmatch = FILE_CONTENTS.search(contents)
        if cmatch is None:
            print(("Error: file {} did not match expected output. Make sure " +
                "that the verification passed.").format(name))
            continue

        v = int(cmatch.group("v"))
        r = int(cmatch.group("r"))
        n = int(cmatch.group("n"))
        variations_ = cmatch.group("variations")
        generate_data = float(cmatch.group("generate_data"))
        generate_adtree = float(cmatch.group("generate_adtree"))
        create_tasks_time = float(cmatch.group("create_tasks_time"))
        process_tasks_time = float(cmatch.group("process_tasks_time"))
        learn_time = float(cmatch.group("learn_time"))
        elapsed_time = float(cmatch.group("elapsed_time"))

        profiling_enabled = cmatch.group("profiling_enabled")
        if profiling_enabled != "false":
            print("Profiling enabled!")

        profiling = cmatch.group("profiling")
        profiles = {}
        if profiling:
            for l in profiling.split("\n"):
                pmatch = PROFILE_LINE.match(l)
                profiles[pmatch.group("id")] = {
                    "n_calls":      int(pmatch.group("n_calls")),
                    "min":          parse_duration(pmatch.group("min")),
                    "max":          parse_duration(pmatch.group("max")),
                    "mad":          parse_duration(pmatch.group("mad")),
                    "mean":         parse_duration(pmatch.group("mean")),
                    "time_percent": int(pmatch.group("time_percent")),
                    "time":         parse_duration(pmatch.group("time")),
                }

        #expected_pars = "-n_{}_-r_{}".format(n, r)
        #if pars != expected_pars:
        #  print(("Error in file {}: parameters were {} according to filename, " +
        #      "but {} according to file contents.").format(name, pars,
        #          expected_pars))

        results.append({
            "variations": variations,
            "alternatives_parallel": alternatives_parallel,
            "v": v,
            "t": t,
            "i": i,

            "generate_data": generate_data,
            "generate_adtree": generate_adtree,
            "create_tasks_time": create_tasks_time,
            "process_tasks_time": process_tasks_time,
            "learn_time": learn_time,
            "elapsed_time": elapsed_time,

            "profile_mean": {id: v["mean"] / 1000.0 for id, v in profiles.items()},
            "profile_total": {id: v["mean"] * v["n_calls"] / 1000.0 for id, v in profiles.items()},
            "profile_n": {id: v["n_calls"] for id, v in profiles.items()},
        })

    return results

def first(tuple):
    return tuple[0]

def collect_bar(raw):
    means = raw[0]["profile_mean"]
    main_phases = [
        sum([means["bayes.main/generate-data"],
             means["bayes.main/generate-adtree"],
             means["bayes.main/score-original"],
             means["bayes.main/alloc-learner"]]),
        means["bayes.main/run-learner"],
        sum([means["bayes.main/check-solution"],
             means["bayes.main/score-solution"]]),
    ]

    main_phases_cumsum = []
    cumsum = 0
    for s in main_phases:
        main_phases_cumsum.append(cumsum)
        cumsum += s

    totals = raw[0]["profile_total"]
    transactional = [
        totals["bayes.learner/find-best-insert-task"], # tx'ional
        totals["bayes.main/run-learner"] - totals["bayes.learner/find-best-insert-task"], # non-tx
    ]

    return {"main_phases": main_phases,
            "main_phases_sum": sum(main_phases),
            "main_phases_cumsum": main_phases_cumsum,
            "transactional": transactional}

def draw_bar(collected):
    fig = plt.figure(figsize=(4, 6), dpi=100)

    PHASES_LABELS = [
        "Generate input",
        "Learn network",
        "Validate solution",
    ]

    bar = plt.axes([0.13, 0.02, 0.15, 0.88]) # left, bottom, width, height
    bar.set_title("Phases of\nthe program", fontsize="large")
    bar.set_xlabel("", fontsize="large")
    bar.set_xlim(0, 1)
    bar.set_xticklabels([])
    bar.set_ylabel("Time (s)", fontsize="large")
    bars = []
    labels = []
    cumsum = collected["main_phases_cumsum"]
    s = collected["main_phases_sum"]
    #bar.set_yticks([0.0, 0.5, 1.0, 1.5, 2.0, 2.5])
    bar.set_ylim(s, 0)
    colors = [sns.crayons["Canary"], sns.crayons["Blue"],
        sns.crayons["Vivid Violet"]]
    hatches = ["/", ".", ""]
    for i, t in enumerate(collected["main_phases"]):
        bars.append(bar.bar(0, height=t, bottom=cumsum[i], width=1,
            color=colors[i], hatch=hatches[i],
            edgecolor=sns.set_hls_values(colors[i], s=1.0, l=0)))
        labels.append("{} ({:.1%})".format(PHASES_LABELS[i], t / s))
    bar.legend(bars, labels,
        #loc=(-0.05, -0.15),
        #loc="lower center", bbox_to_anchor=(0.5, -0.15),
        loc=(1.0, 0.0),
        prop={"size": "large"})

    # width = 0.5*4 = 2.0
    # height = 2.0 = 0.33*6
    pie = plt.axes([0.43, 0.54, 0.5, 0.33]) # left, bottom, width, height
    pie.set_title("Time spent\nin transaction\n(in learning phase)",
        fontsize="large")
    colors = [sns.crayons["Red Orange"], sns.crayons["Silver"]]
    tx = collected["transactional"]
    labels = ["Transactional ({:.1%})".format(tx[0] / sum(tx)),
              "Non-transactional ({:.1%})".format(tx[1] / sum(tx))]
    patches, _texts = pie.pie(tx,
        #labels=labels,
        #labeldistance=0.1,
        colors=colors,
        startangle=90)
    patches[0].set_hatch("+")
    patches[0].set_edgecolor(sns.set_hls_values(colors[0], s=1.0, l=0))
    patches[1].set_hatch("x")
    patches[1].set_edgecolor(sns.set_hls_values(colors[1], s=0.0, l=0))
    pie.legend(patches, labels,
        loc="lower center", bbox_to_anchor=(0.5, -0.3),
        prop={"size": "large"})
    plt.savefig("bayes-bar.pdf", bbox_inches="tight")
    #plt.show()

def collect_speedup(raw):
    times_by_variation = defaultdict(lambda: defaultdict(list))
    for r in raw:
        if r["variations"] == "" or r["variations"] == "original":
            variation = ""
        elif r["variations"] == "alternatives-parallel":
            variation = "parallel-for"
        else:
            print("Error: unknown variation {}.".format(variation))
            continue
        times_by_variation[variation][r["t"]].append(r["process_tasks_time"])

    quartiles_by_variation = defaultdict(OrderedDict)
    for (variation, times_by_t) in times_by_variation.items():
        quartiles_by_t = {}
        for (t, times) in times_by_t.items():
            quartiles_by_t[t] = {
                "first":  np.percentile(times, 25),
                "median": np.median(times),
                "third":  np.percentile(times, 75),
            }

        quartiles_by_variation[variation] = \
            OrderedDict(sorted(quartiles_by_t.items(), key=first))
    print(quartiles_by_variation)

    # Speed-up is compared to t=1 in original version
    base = quartiles_by_variation[""][1]["median"]
    def speedup(t):
        return base / t

    median_speedups_by_variation = defaultdict(OrderedDict)
    errors_by_variation = defaultdict(OrderedDict)
    for (variation, quartiles_by_t) in quartiles_by_variation.items():
        max_t = None
        max_speedup = 0
        max_time = 0
        for (t, quartiles) in quartiles_by_t.items():
            fst = quartiles["first"]
            med = quartiles["median"]
            thd = quartiles["third"]
            # Calculate median
            median_speedups_by_variation[variation][t] = speedup(med)
            # Find maximum
            if speedup(med) > max_speedup:
                max_t = t
                max_speedup = speedup(med)
                max_time = med
            # Calculate errors
            # Watch out: higher time is lower speedup
            # => first quartile in time is third quartile in speedup
            error_fst = speedup(med) - speedup(thd)
            error_thd = speedup(fst) - speedup(med)
            errors_by_variation[variation][t] = [error_fst, error_thd]
        print("For variation '{}', at t = {} a maximal speedup of {} is "
            "reached, corresponding to a run time of {} ms.".format(variation,
            max_t, max_speedup, max_time))

    return {"median_speedups": median_speedups_by_variation,
            "errors":          errors_by_variation,
            "quartiles":       quartiles_by_variation}

def is_power_of_two(x):
    return math.log(x, 2) % 1 == 0.0

def draw_speedup(collected):
    # Type 1 fonts
    plt.rcParams["ps.useafm"] = True
    plt.rcParams["pdf.use14corefonts"] = True
    #plt.rcParams["text.usetex"] = True

    sns.set_style("whitegrid", {"grid.color": ".9"})

    variations = collected["median_speedups"].keys()
    LABELS = {
        "":             "Original version",
        "parallel-for": "Parallel loop ({} tasks\nper worker thread)".format(32),
    }
    COLORS = {
        "":             "#003399",
        "parallel-for": "#FF6600",
    }

    ax = plt.axes()
    sns.despine(top=True, right=True, left=True, bottom=True)

    #ax.set_title("Measured speed-up", fontsize="x-large")

    ax.set_xlabel(r"Max number of parallel transactions ($t$)", fontsize="large")
    ax.set_xscale("log", basex=2)
    xticks = [x for x in collected["median_speedups"][""].keys() if is_power_of_two(x)]
    ax.set_xticks(xticks)
    ax.set_xticklabels(xticks)
    ax.set_xlim(0.99, 129)

    ax.set_ylabel("Speed-up", fontsize="large")
    ax.set_ylim(0, 4.01)
    #ax.set_yticks([0.0, 1.0, 2.0, 3.0])

    lines = {}
    for (variation, series) in collected["median_speedups"].items():
        x = [t for t in series.keys()]
        median_speedups = list(collected["median_speedups"][variation].values())
        errors = np.transpose(list(collected["errors"][variation].values()))
        line = ax.errorbar(x=x, y=median_speedups, yerr=errors,
            color=COLORS[variation])
        lines[variation] = line

    ax.legend([lines[v] for v in variations], [LABELS[v] for v in variations],
        loc="lower right", prop={"size": "small"})

    arrowprops = {
        "arrowstyle": "->",
        "color": "black",
        "connectionstyle": "angle3,angleA=0,angleB=90",
        "shrinkB": 5,
    }
    # print(collected["median_speedups"][""][1])
    # print(collected["quartiles"][""][1]["median"])
    ax.annotate(xy=(1, 1.0), s="original version for $t = 1$:\ntime = 3360 ms",
        xytext=(40, -10), textcoords="offset points", arrowprops=arrowprops)

    # print(collected["median_speedups"][""][112])
    # print(collected["quartiles"][""][112]["median"])
    ax.annotate(xy=(112, 2.55), s="maximum for $t = 112$:\nspeed-up = 2.81\ntime = 1195 ms",
        xytext=(-110, -70), textcoords="offset points", arrowprops=arrowprops)

    # print(collected["median_speedups"]["parallel-for"][1])
    # print(collected["quartiles"]["parallel-for"][1]["median"])
    ax.annotate(xy=(1, 2.1), s="parallel loop for $t = 1$:\nspeed-up = 2.07\ntime = 1622 ms",
        xytext=(5, 50), textcoords="offset points", arrowprops=arrowprops)

    # print(collected["median_speedups"]["parallel-for"][5])
    # print(collected["quartiles"]["parallel-for"][5]["median"])
    ax.annotate(xy=(5, 3.35), s="maximum for $t = 5$:\nspeed-up = 3.33\ntime = 1008 ms",
        xytext=(20, 10), textcoords="offset points", arrowprops=arrowprops)

    plt.savefig("bayes-speedup-{}.pdf".format(PATH), bbox_inches="tight")
    #plt.show()

if __name__ == "__main__":
    raw_results = parse_files()
    #print(raw_results)

    # results_bar = collect_bar(raw_results)
    # print(results_bar)
    # draw_bar(results_bar)

    results_speedup = collect_speedup(raw_results)
    print(results_speedup)
    draw_speedup(results_speedup)
