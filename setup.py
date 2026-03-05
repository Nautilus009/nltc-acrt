from pathlib import Path
from setuptools import setup, find_packages

ROOT = Path(__file__).resolve().parent
VERSION_NS = {}
exec((ROOT / "src" / "acrt_pkg" / "__init__.py").read_text(), VERSION_NS)

setup(
    name="nltc-acrt",
    version=VERSION_NS["__version__"],
    description="ACRT COBOL auditing CLI",
    long_description=(ROOT / "README.md").read_text(encoding="utf-8"),
    long_description_content_type="text/markdown",
    license="Internal",
    python_requires=">=3.7",
    package_dir={"": "src"},
    packages=find_packages(where="src"),
    py_modules=["acrt"],
    entry_points={
        "console_scripts": [
            "acrt=acrt:main",
        ],
    },
)
