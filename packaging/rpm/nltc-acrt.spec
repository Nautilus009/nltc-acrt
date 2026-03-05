Name:           nltc-acrt
Version:        0.4.1
Release:        1%{?dist}
Summary:        ACRT COBOL auditing CLI

License:        Internal
URL:            https://internal/nltc-acrt
Source0:        %{name}-%{version}.tar.gz
BuildArch:      noarch

BuildRequires:  python3
BuildRequires:  python3-setuptools
Requires:       python3 >= 3.7

%description
nltc-acrt is a COBOL audit CLI that compares Master and Local/Private
compilation listing files and reports newly introduced violations.

%prep
%setup -q

%build
# No compilation required for pure-Python package.

%install
rm -rf %{buildroot}
python3 setup.py install --root %{buildroot} --prefix %{_prefix}

%files
%license LICENSE
%doc README.md CHANGELOG.md
%{_bindir}/acrt
%{python3_sitelib}/acrt.py*
%{python3_sitelib}/__pycache__/acrt*.pyc
%{python3_sitelib}/acrt_pkg/
%{python3_sitelib}/nltc_acrt-*.egg-info/

%changelog
* Thu Mar 05 2026 NLTC Packaging Team <noreply@nltc> - 0.4.2-1
- Release 0.4.2 with RPM packaging/release pipeline updates

* Wed Mar 04 2026 NLTC Packaging Team <noreply@nltc> - 0.4.1-1
- Initial RPM packaging
