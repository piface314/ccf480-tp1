import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from sys import argv
import os


def f1(x, y):
    return np.sin(x + y) + (x - y) ** 2 - 1.5 * x + 2.5 * y + 1.0


def f2(x, y):
    y47 = 47 + y
    return -y47 * np.sin(np.sqrt(np.abs(x / 2 + y47))) - x * np.sin(np.sqrt(np.abs(x - y47)))


fns = {"1a": f1, "1b": f1, "2c": f2, "2d": f2}

points = {"HC": {"c": "r", "marker": "o"},
          "ILS": {"c": "b", "marker": "s"}}

limits = {"1a": [(-1.5, 4.0), (-3.0, 4.0), 0.025],
          "1b": [(-1.0, 0.0), (-2.0, -1.0), 0.025],
          "2c": [(-512.0, 512.0), (-512.0, 512.0), 1.0],
          "2d": [(511.0, 512.0), (404.0, 405.0), 0.001]}


def describe(df):
    desc = df.groupby(by=["case", "algorithm"])[["z"]].describe()
    print(desc)


def box(df, case, figsdir):
    df[df.case == case].boxplot(column="z", by="algorithm")
    plt.suptitle("")
    plt.subplots_adjust(bottom=.2, left=.2)
    plt.ylabel("z(x,y)")
    plt.xlabel("Algoritmo")
    plt.title(f"Valores da função objetivo encontradas do Caso {case}")
    plt.savefig(os.path.join(figsdir, f"box-{case}.png"))
    plt.clf()


def contour(df, figsdir):
    cases = sorted(df["case"].unique())
    algs = sorted(df["algorithm"].unique())
    for case in cases:
        cdf = df[df.case == case]
        box(cdf, case, figsdir)
        ax = plt.subplot()
        lim = limits[case]
        xlim, ylim, delta = lim[0], lim[1], lim[2]
        _x = np.arange(xlim[0], xlim[1], delta)
        _y = np.arange(ylim[0], ylim[1], delta)
        x, y = np.meshgrid(_x, _y)
        z = fns[case](x, y)
        cs = ax.contour(x, y, z, linewidths=0.6)
        ax.clabel(cs, inline=True, fontsize=8)
        plt.colorbar(cs)
        for alg in algs:
            cadf = cdf[cdf.algorithm == alg]
            cadf.plot.scatter("x", "y", s=1, ax=ax, label=alg, **points[alg])
        plt.title(f"Soluções encontradas no Caso {case}")
        plt.legend()
        plt.xlim(*xlim)
        plt.ylim(*ylim)
        plt.savefig(os.path.join(figsdir, f"contour-{case}.png"))
        plt.clf()


if __name__ == "__main__":
    if len(argv) >= 2:
        fp = argv[1]
        figsdir = argv[2]
        df = pd.read_csv(fp)
        describe(df)
        contour(df, figsdir)
    else:
        print("Faltando arquivo de entrada.")
