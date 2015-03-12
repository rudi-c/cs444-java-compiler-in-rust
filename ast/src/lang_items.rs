use middle::*;
use name::Symbol;

pub struct LangItems<'a, 'ast: 'a> {
    /// `java.lang`
    pub lang: PackageRef<'a, 'ast>,
    /// `java.lang.Object`
    pub object: TypeDefinitionRef<'a, 'ast>,
    /// `java.lang.Cloneable`
    pub cloneable: TypeDefinitionRef<'a, 'ast>,
    /// `java.lang.String`
    pub string: TypeDefinitionRef<'a, 'ast>,
    /// `java.io.Serializable`
    pub serializable: TypeDefinitionRef<'a, 'ast>,
}

macro_rules! get {
    ($package: ident[$name: expr] = $kind: path) => ({
        let package = $package;
        match package.contents.borrow().get(&Symbol::from_str($name)) {
            Some(&$kind(x)) => x,
            _ => panic!("incomplete stdlib: `{}` not found in `{}`", $name, package.fq_name),
        }
    });
}

pub fn find_lang_items<'a, 'ast>(toplevel: PackageRef<'a, 'ast>) -> LangItems<'a, 'ast> {
    use middle::PackageItem::*;
    let java = get!(toplevel["java"] = Package);
    let lang = get!(java["lang"] = Package);
    let object = get!(lang["Object"] = TypeDefinition);
    let cloneable = get!(lang["Cloneable"] = TypeDefinition);
    let string = get!(lang["String"] = TypeDefinition);
    let io = get!(java["io"] = Package);
    let serializable = get!(io["Serializable"] = TypeDefinition);
    LangItems {
        lang: lang,
        object: object,
        cloneable: cloneable,
        string: string,
        serializable: serializable,
    }
}

