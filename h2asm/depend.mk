error.cmo: error.cmi
error.cmx: error.cmi
lex.cmo: st.cmi parse.cmi error.cmi lex.cmi
lex.cmx: st.cmx parse.cmx error.cmx lex.cmi
main.cmo: translate.cmi st.cmi parse.cmi lex.cmi error.cmi main.cmi
main.cmx: translate.cmx st.cmx parse.cmx lex.cmx error.cmx main.cmi
parse.cmo: st.cmi error.cmi parse.cmi
parse.cmx: st.cmx error.cmx parse.cmi
st.cmo: error.cmi st.cmi
st.cmx: error.cmx st.cmi
translate.cmo: st.cmi error.cmi translate.cmi
translate.cmx: st.cmx error.cmx translate.cmi
