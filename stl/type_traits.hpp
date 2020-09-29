/// [meta.type.synop]

#ifndef TINY_STL_TYPE_TRAITS_HPP
#define TINY_STL_TYPE_TRAITS_HPP

namespace tiny_stl {
  // [meta.help], helper typename

  template<typename T, T v> struct integral_constant {
    static constexpr T value = v;

    using value_type = T;
    using type = integral_constant<T, v>;

    constexpr operator value_type() const { return value; }
    constexpr value_type operator()() const { return value; }
  };

  template<bool B>
    using bool_constant = integral_constant<bool, B>;
  using true_type  = bool_constant<true>;
  using false_type = bool_constant<false>;

  // [meta.unary.cat], primary type categories

  namespace detail {
    template<typename T> struct remove_cv {
      using type = T;
      template<template<typename> typename Fn>
        using apply = Fn<T>;
    };
    template<typename T> struct remove_cv<T const> {
      using type = T;
      template<template<typename> typename Fn>
        using apply = Fn<T> const;
    };
    template<typename T> struct remove_cv<T const volatile> {
      using type = T;
      template<template<typename> typename Fn>
        using apply = Fn<T> const volatile;
    };
    template<typename T> struct remove_cv<T volatile> {
      using type = T;
      template<template<typename> typename Fn>
        using apply = Fn<T> volatile;
    };

    template<typename T>
      using remove_cv_t = typename remove_cv<T>::type;

    template<typename T, typename U> struct is_same
      : ::tiny_stl::false_type {};
    template<typename T> struct is_same<T, T>
      : ::tiny_stl::true_type {};

    template<typename T, typename U>
      inline constexpr bool is_same_v = ::tiny_stl::detail::is_same<T, U>::value;
  } // namespace tiny_stl::detail

  // T is void
  template<typename T> struct is_void
    : detail::is_same<void, detail::remove_cv_t<T>> {};

  // T is nullptr_t ([basic.fundamental])
  template<typename T> struct is_null_pointer
    : detail::is_same<decltype(nullptr), detail::remove_cv_t<T>> {};

  // T is an integral type ([basic.fundamental])
  template<typename T> struct is_integral
    : bool_constant<
        detail::is_same_v<detail::remove_cv_t<T>, bool> ||
        detail::is_same_v<detail::remove_cv_t<T>, char> ||
        detail::is_same_v<detail::remove_cv_t<T>, signed char> ||
        detail::is_same_v<detail::remove_cv_t<T>, unsigned char> ||
        detail::is_same_v<detail::remove_cv_t<T>, wchar_t> ||
        detail::is_same_v<detail::remove_cv_t<T>, char8_t> ||
        detail::is_same_v<detail::remove_cv_t<T>, char16_t> ||
        detail::is_same_v<detail::remove_cv_t<T>, char32_t> ||
        detail::is_same_v<detail::remove_cv_t<T>, short int> ||
        detail::is_same_v<detail::remove_cv_t<T>, int> ||
        detail::is_same_v<detail::remove_cv_t<T>, long int> ||
        detail::is_same_v<detail::remove_cv_t<T>, long long int> ||
        detail::is_same_v<detail::remove_cv_t<T>, unsigned short int> ||
        detail::is_same_v<detail::remove_cv_t<T>, unsigned int> ||
        detail::is_same_v<detail::remove_cv_t<T>, unsigned long int> ||
        detail::is_same_v<detail::remove_cv_t<T>, unsigned long long int>
    > {};

  // T is a floating-point type ([basic.fundamental])
  template<typename T> struct is_floating_point
    : bool_constant<
        detail::is_same_v<detail::remove_cv_t<T>, float> ||
        detail::is_same_v<detail::remove_cv_t<T>, double> ||
        detail::is_same_v<detail::remove_cv_t<T>, long double>
    > {};

  // T is an array type ([basic.compound]) of known or unknown extent
  template<typename T> struct is_array
    : false_type {};
  template<typename T> struct is_array<T[]>
    : true_type {};
  template<typename T, size_t N> struct is_array<T[N]>
    : true_type {};

  // T is a pointer type ([basic.compound])
  template<typename T> struct is_pointer;

  namespace detail {
    template<typename T> struct is_pointer_helper
      : ::tiny_stl::false_type {};
    template<typename T> struct is_pointer_helper<T*>
      : ::tiny_stl::true_type {};
  } // namespace tiny_stl::detail

  template<typename T> struct is_pointer
    : detail::is_pointer_helper<detail::remove_cv_t<T>> {};

  // T is an lvalue reference type ([dcl.ref])
  template<typename T> struct is_lvalue_reference
    : false_type {};
  template<typename T> struct is_lvalue_reference<T&>
    : true_type {};

  // T is an rvalue reference type ([dcl.ref])
  template<typename T> struct is_rvalue_reference
    : false_type {};
  template<typename T> struct is_rvalue_reference<T&&>
    : true_type {};

  // T is a pointer to data member
  template<typename T> struct is_member_object_pointer;

  // T is a pointer to member function
  template<typename T> struct is_member_function_pointer;

  // T is an enumeration type ([basic.compound])
  template<typename T> struct is_enum
    : bool_constant<__is_enum(T)> {};

  // T is a union type ([basic.compound])
  template<typename T> struct is_union
    : bool_constant<__is_union(T)> {};

  // T is a non-union typename type ([basic.compound])
  template<typename T> struct is_class;

  namespace detail {
    template<typename T> auto test_is_class(int T::*)
      -> ::tiny_stl::bool_constant<!::tiny_stl::is_union<T>::value>;
    template<typename T> auto test_is_class(...)
      -> ::tiny_stl::false_type;
  } // namespace tiny_stl::detail

  template<typename T> struct is_class
    : decltype(detail::test_is_class<T>(nullptr)) {};

  // T is a function type ([basic.compound])
  template<typename T> struct is_function
    : false_type {};

  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) const noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) const noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) const volatile noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) const volatile noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) volatile noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) volatile noexcept(NE)> : true_type {};

  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) & noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) & noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) const & noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) const & noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) const volatile & noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) const volatile & noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) volatile & noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) volatile & noexcept(NE)> : true_type {};

  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) && noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) && noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) const && noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) const && noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) const volatile && noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) const volatile && noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args...) volatile && noexcept(NE)> : true_type {};
  template<typename Ret, typename... Args, bool NE>
    struct is_function<Ret(Args......) volatile && noexcept(NE)> : true_type {};

  // [meta.unary.comp], composite type categories

  // T is an lvalue reference or an rvalue reference
  template<typename T> struct is_reference 
    : bool_constant<is_lvalue_reference<T>::value || is_rvalue_reference<T>::value> {};

  // T is an arithmetic type ([basic.fundamental])
  template<typename T> struct is_arithmetic
    : bool_constant<is_integral<T>::value || is_floating_point<T>::value> {};

  // T is a fundamental type ([basic.fundamental])
  template<typename T> struct is_fundamental
    : bool_constant<is_arithmetic<T>::value || is_void<T>::value || is_null_pointer<T>::value> {};

  // T is an object type ([basic.types])
  template<typename T> struct is_object
    : bool_constant<!is_function<T>::value && !is_reference<T>::value && !is_void<T>::value> {};

  // T is a scalar type ([basic.types])
  template<typename T> struct is_scalar
    : bool_constant<is_object<T>::value && !is_array<T>::value && !is_class<T>::value && !is_union<T>::value> {};

  // T is a compound type ([basic.compound])
  template<typename T> struct is_compound
    : bool_constant<!is_fundamental<T>::value> {};

  // T is a pointer-to-member type
  template<typename T> struct is_member_pointer;

  namespace detail {
    template<class T> struct is_member_pointer_helper
      : ::tiny_stl::false_type {};
    template<class T, class U> struct is_member_pointer_helper<T U::*>
      : ::tiny_stl::true_type {};

    template<class T> struct is_member_function_pointer_helper
      : ::tiny_stl::false_type {};
    template<class T, class U> struct is_member_function_pointer_helper<T U::*>
      : ::tiny_stl::is_function<T> {};
  } // namespace tiny_stl::detail

  template<typename T> struct is_member_pointer
    : detail::is_member_pointer_helper<detail::remove_cv_t<T>> {};

  template<class T> struct is_member_function_pointer
    : detail::is_member_function_pointer_helper<detail::remove_cv_t<T>> {};

  template<class T> struct is_member_object_pointer
    : bool_constant<is_member_pointer<T>::value && !is_member_function_pointer<T>::value> {};

  // [meta.unary.prop], type properties

  // T is const-qualified ([basic.type.qualifier])
  template<typename T> struct is_const
    : false_type {};
  template<typename T> struct is_const<T const>
    : true_type {};

  // T is volatile-qualified ([basic.type.qualifier])
  template<typename T> struct is_volatile
    : false_type {};
  template<typename T> struct is_volatile<T volatile>
    : true_type {};

  // TODO
  // T is a trivial type ([basic.types])
  template<typename T> struct is_trivial;

  // TODO
  // T is a trivially copyable type ([basic.types])
  template<typename T> struct is_trivially_copyable;

  // TODO
  // T is a standard-layout type ([basic.types])
  template<typename T> struct is_standard_layout;

  // TODO
  // T is a typename type, but not a union type, with no non-static data members
  // other than subobjects of zero size, no virtual member functions, no virtual
  // base typenamees, and no base typename B for which is_empty_v<B> is false.
  template<typename T> struct is_empty;

  // TODO
  // T is a polymorphic typename ([typename.virtual])
  template<typename T> struct is_polymorphic;

  // T is an abstract typename ([typename.abstract])
  template<typename T> struct is_abstract
    : bool_constant<__is_abstract(T)> {};

  // T is a typename type marked with the typename-virt-specifier final ([typename.pre]).
  template<typename T> struct is_final
    : bool_constant<__is_final(T)> {};

  // TODO
  // T is an aggregate type ([dcl.init.aggr])
  template<typename T> struct is_aggregate;

  // If is_arithmetic_v<T> is true, the same result as T(-1) < T(0);
  // otherwise, false
  template<typename T> struct is_signed;

  namespace detail {
    template<typename T, bool = ::tiny_stl::is_arithmetic<T>::value> struct is_signed_helper
      : ::tiny_stl::bool_constant<T(-1) < T(0)> {};

    template<typename T> struct is_signed_helper<T, false>
      : ::tiny_stl::false_type {};
  } // namespace tiny_stl::detail

  template<typename T> struct is_signed
    : detail::is_signed_helper<T> {};

  // If is_arithmetic_v<T> is true, the same result as T(0) < T(-1);
  // otherwise, false
  template<typename T> struct is_unsigned;

  namespace detail {
    template<typename T, bool = ::tiny_stl::is_arithmetic<T>::value> struct is_unsigned_helper
      : ::tiny_stl::bool_constant<T(0) < T(-1)> {};

    template<typename T> struct is_unsigned_helper<T, false>
      : ::tiny_stl::false_type {};
  } // namespace tiny_stl::detail

  template<typename T> struct is_unsigned
    : detail::is_unsigned_helper<T> {};

  // T is an array type of known bound ([dcl.array])
  template<typename T> struct is_bounded_array
    : false_type {};
  template<typename T, size_t N> struct is_bounded_array<T[N]>
    : true_type {};

  // T is an array type of unknown bound ([dcl.array])
  template<typename T> struct is_unbounded_array
    : false_type {};
  template<typename T> struct is_unbounded_array<T[]>
    : true_type {};

  // TODO
  // For a function type T or for a cv void type T, is_constructible_v<T, 
  // Args...> is false, otherwise
  // The predicate condition for a template specialization is_constructible<T, Args...> shall be satisfied if and only
  // if the following variable defination would be well-formed for some invented variable t:
  //    T t(declval<Args>()...);
  // Access checking is performed as if in a context unrelated to T and any of the Args. Only the validity of the immediate
  // context of the variable initialization is considered.
  template<typename T, typename... Args> struct is_constructible;

  // is_constructible_v<T> is true.
  template<typename T> struct is_default_constructible
    : is_constructible<T> {};

  // For a referenceable type T ([defns.referenceable]), the same result as
  // is_constructible_v<T, const T&>, otherwise false.
  template<typename T> struct is_copy_constructible;

  // For a referenceable type T, the same result as is_constructible_v<T,
  // T&&>, otherwise false.
  template<typename T> struct is_move_constructible;

  // TODO
  // The expression declval<T>() = declval<U>() is well-formed when
  // treated as an unevaluated operand. Access checking is performed as if in a
  // context unrelated to T and U. Only the validity of the immediate context of
  // the assignment expression is considered.
  template<typename T, typename U> struct is_assignable;

  // For a referenceable type T, the same result as is_assignable_v<T&,
  // const T&>, otherwise false.
  template<typename T> struct is_copy_assignable;

  // For a referenceable type T, the same result as is_assignable_v<T&,
  // T&&>, otherwise false.
  template<typename T> struct is_move_assignable;

  // TODO
  // The expressions swap(declval<T>() declval<U>()) and
  // swap(declval<U>(), declval<T>()) are each well-formed when treated
  // as an unevaluated operand in an overload-resolution context for swappable
  // values ([swappable.requirements]). Access checking is performed as if in a
  // context unrelated to T and U. Only the validity of the immediate context of
  // the swap expressions is considered.
  template<typename T, typename U> struct is_swappable_with;

  // For a referenceable type T, the same result as is_swappable_with_v<T&,
  // T&>, otherwise false.
  template<typename T> struct is_swappable;

  // TODO
  // Either T is a reference type, or T is a complete object type for which the 
  // expression declval<U&>().~U() is well-formed when threated as an
  // unevaluated operand, whrere U is remove_all_extents_t<T>.
  template<typename T> struct is_destructible;

  // TODO
  // is_constructible_v<T, 
  // Args...> is true and the variable definition for is_constructible, as
  // defined below, is known to call no operation that is not trivial ([basic.
  // types], [special])
  template<typename T, typename... Args> struct is_trivially_constructible;

  // is_trivially_constructible_v<T> is true;
  template<typename T> struct is_trivially_default_constructible
    : is_trivially_constructible<T> {};

  // For a referenceable type T, the same result as
  // is_trivially_constructible_v<T, const T&>, otherwise false.
  template<typename T> struct is_trivially_copy_constructible;

  // For a referenceable type T, the same result as
  // is_trivially_constructible_v<T, T&&>, otherwise false.
  template<typename T> struct is_trivially_move_constructible;

  // TODO
  // is_assignable_v<T, U> is true and the assignment, as defined by
  // is_assignable, is known to call no operation that is not trivial ([basic.
  // types], [special]).
  template<typename T, typename U> struct is_trivially_assignable;

  // For a referenceable type T, the same result as
  // is_trivially_assignable_v<T&, const T&>, otherwise false.
  template<typename T> struct is_trivially_copy_assignable;

  // For a referenceable type T, the same result as
  // is_trivially_assignable_v<T&, T&&>, otherwise false.
  template<typename T> struct is_trivially_move_assignable;

  // TODO
  // is_destructible_v<T> is true and remove_all_extents_t<T> is either
  // a non-typename type or a typename type with a trivial destructor.
  template<typename T> struct is_trivially_destructible;

  // TODO
  // is_­constructible_­v<T, Args...> is true and the variable definition for 
  // is_­constructible, as defined below, is known not to throw any 
  // exceptions ([expr.unary.noexcept]).
  template<typename T, typename... Args> struct is_nothrow_constructible;

  // is_­nothrow_­constructible_­v<T> is true.
  template<typename T> struct is_nothrow_default_constructible
    : is_nothrow_constructible<T> {};

  // For a referenceable type T, the same result as
  // is_nothrow_constructible_v<T, const T&>, otherwise false.
  template<typename T> struct is_nothrow_copy_constructible;

  // For a referenceable type T, the same result as
  // is_nothrow_constructible_v<T, T&&>, otherwise false.
  template<typename T> struct is_nothrow_move_constructible;

  // TODO
  // is_assignable_v<T, U> is true and the assignment is known not to
  // throw any exceptions ([expr.unary.noexcept]).
  template<typename T, typename U> struct is_nothrow_assignable;

  // For a referenceable type T, the same result as
  // is_no_throw_assignable_v<T, const T&>, otherwise false.
  template<typename T> struct is_nothrow_copy_assignable;

  // For a referenceable type T, the same result as
  // is_no_throw_assignable_v<T, T&&>, otherwise false.
  template<typename T> struct is_nothrow_move_assignable;

  // TODO
  // is_swappable_with_v<T, U> is true and each swap expression of the
  // definition of is_swappable_with<T, U> is known not to throw any
  // exceptions ([expr.unary.noexcept]).
  template<typename T, typename U> struct is_nothrow_swappable_with;

  // For a referenceable type T, the same result as
  // is_nothrow_swappable_with_v<T&, T&>, otherwise false.
  template<typename T> struct is_nothrow_swappable;

  // TODO
  // is_destructible_v<T> is true and the indicated destructor is known not
  // to throw any exceptions ([expr.unary.noexcept]).
  template<typename T> struct is_nothrow_destructible;

  // TODO
  // T has a virtual destructor ([typename.dtor])
  template<typename T> struct has_virtual_destructor;

  // TODO
  // For an array type T, the same result as
  // has_unique_object_representations_v<remove_all_extents_t<T>>,
  // otherwise
  // The predicate condition for a template specialization has_unique_object_representations<T> shall be satisfied if
  // and only if:
  //  - T is trivially copyable, and
  //  - any two objects of type T with the same value have the same object representation, where two objects of array or
  //    non-union typename type are considered to have the same value if their respective sequences of direct subjects
  //    have the same values, and two objects of union type are considered to have the same value if they have the same
  //    active member and the corresponding members have the same value.
  // The set of scalar types for which this condition holds is implementation-defined.
  template<typename T> struct has_unique_object_representations;

  // [meta.unary.prop.query], type property queires

  // alignof(T).
  template<typename T> struct alignment_of
    : integral_constant<size_t, alignof(T)> {};

  // If T names an array type, an integer value representing the number of
  // dimensions of T; otherwise, 0.
  template<typename T> struct rank
    : integral_constant<size_t, 0> {};

  template<typename T> struct rank<T[]>
    : integral_constant<size_t, rank<T>::value + 1> {};
  template<typename T, size_t N> struct rank<T[N]>
    : integral_constant<size_t, rank<T>::value + 1> {};

  // If T is not an array type, or if it has rank less than or equal to T, or if I is 0 and T
  // has type "array of unknown bound of U", then 0; otherwise, the bound ([dcl.
  // array]) of the I^th dimention of T, where indexing of I is zero-based
  template<typename T, unsigned I = 0> struct extent
    : integral_constant<size_t, 0> {};

  template<typename T> struct extent<T[], 0>
    : integral_constant<size_t, 0> {};
  template<typename T, unsigned I> struct extent<T[], I>
    : extent<T, I - 1> {};

  template<typename T, size_t N> struct extent<T[N], 0>
    : integral_constant<size_t, N> {};
  template<typename T, size_t N, unsigned I> struct extent<T[N], I>
    : extent<T, I - 1> {};

  // [meta.rel], type relations

  // T and U name the same type
  // with the same cv-qualifications
  template<typename T, typename U> struct is_same
    : false_type {};
  template<typename T> struct is_same<T, T>
    : true_type {};

  // TODO
  // Base is a base class of Derived ([class.derived]) without regard
  // to cv-qualifiers or Base and Derived are not unions and
  // name the same class type without regard to cv-qualifiers
  template<typename Base, typename Derived> struct is_base_of;

  namespace detail {

  } // namespace tiny_stl::detail

  // TODO
  // The predicate condition for a template specialization is_convertible<From, To> shall be satisfied if and only if the
  // return expression in the following code would be well-formed, including any implicit conversions to the return type
  // of the function:
  //   To test() {
  //     return declval<From>();
  //   }
  // Access checking is performed in a context unrelated to To and From. Only the validity of the immediate context of the
  // expression of the return statement ([stmt.return]) (including initialization of the returned object or reference) is
  // considered.
  template<typename From, typename To> struct is_convertible;

  // TODO
  // is_convertible_v<From, To> is true and the conversion, as
  // defined by is_convertible, is known not to throw any
  // exceptions ([expr.unary.noexcept])
  template<typename From, typename To> struct is_nothrow_convertible;

  // TODO
  // T and U are layout-compatible ([basic.types])
  template<typename T, typename U> struct is_layout_compatible;

  // TODO
  // Derived is unambiguously derived from Base without
  // regard to cv-qualifiers, and each object of type Derived is
  // pointer-interconvertible ([basic.compound]) with its Base
  // subobject, or Base and Derived are not unions and name the
  // same class type without regard to cv-qualifiers.
  template<typename Base, typename Derived> struct is_pointer_interconvertible_base_of;

  // TODO
  // The expression INVOKE(declval<Fn>(), declval<ArgTypes>)()...) is well-formed when treated as an
  // unevaluated operand
  template<typename Fn, typename... ArgTypes> struct is_invocable;

  // TODO
  // The expression INVOKE<R>(declval<Fn>(), declval<ArgTypes>)()...) is well-formed when treated as an
  // unevaluated operand.
  template<typename R, typename Fn, typename... ArgTypes> struct is_invocable_r;

  // TODO
  // is_invocable_v<Fn, ArgTypes...> is true and the expression INVOKE(declval<Fn>(),
  // declval<ArgTypes>()...) is known not to throw any exceptions ([expr.unary.noexcept])
  template<typename Fn, typename... ArgTypes> struct is_nothrow_invocable;

  // TODO
  // is_invocable_r_v<R, Fn, ArgTypes...> is true and the expression INVOKE<R>(declval<Fn>(),
  // declval<ArgTypes>()...) is known not to throw any exceptions ([expr.unary.noexcept])
  template<typename R, typename Fn, typename... ArgTypes> struct is_nothrow_invocable_r;

  // [meta.trans.cv], const-volatile modifications

  // The member typedef type names the same type as T except that any top-
  // level const-qualifier has been removed.
  template<typename T> struct remove_const          { typedef T type; };
  template<typename T> struct remove_const<T const> { typedef T type; };

  // The member typedef type names the same type as T except that any top-
  // level volatile-qualifier has been removed.
  template<typename T> struct remove_volatile             { typedef T type; };
  template<typename T> struct remove_volatile<T volatile> { typedef T type; };

  // The member typedef type shall be the same as T except that any top-level
  // cv-qualifier has been removed.
  template<typename T> struct remove_cv                   { typedef T type; };
  template<typename T> struct remove_cv<T const>          { typedef T type; };
  template<typename T> struct remove_cv<T const volatile> { typedef T type; };
  template<typename T> struct remove_cv<T volatile>       { typedef T type; };

  // If T is a reference, function, or top-level const-qualified type, then type
  // names the same type as T, otherwise T const.
  template<typename T> struct add_const { typedef T const type; };

  // If T is a reference, function, or top-level const-qualified type, then type
  // names the same type as T, otherwise T volatile.
  template<typename T> struct add_volatile { typedef T volatile type; };

  // The member typedef type names the same type as
  // add_const_t<add_volatile_t<T>>.
  template<typename T> struct add_cv { typedef T const volatile type; };

  template<typename T>
    using remove_const_t    = typename remove_const<T>::type;
  template<typename T>
    using remove_volatile_t = typename remove_volatile<T>::type;
  template<typename T>
    using remove_cv_t       = typename remove_cv<T>::type;
  template<typename T>
    using add_const_t       = typename add_const<T>::type;
  template<typename T>
    using add_volatile_t    = typename add_volatile<T>::type;
  template<typename T>
    using add_cv_t          = typename add_cv<T>::type;

  // [meta.trans.ref], reference modifications

  // If T has type "reference to T1" then the member typedef type names
  // T1; otherwise, type names T.
  template<typename T> struct remove_reference      { using type = T; };
  template<typename T> struct remove_reference<T&>  { using type = T; };
  template<typename T> struct remove_reference<T&&> { using type = T; };

  // If T names a referenceable type then the member typedef type names
  // T&; otherwise, type names T;
  template<typename T> struct add_lvalue_reference;

  namespace detail {
    template<typename T> struct type_identity { using type = T; };

    template<typename T> auto try_add_lvalue_reference(int)
      -> ::tiny_stl::detail::type_identity<T&>;
    template<typename T> auto try_add_lvalue_reference(...)
      -> ::tiny_stl::detail::type_identity<T>;

    template<typename T> auto try_add_rvalue_reference(int)
      -> ::tiny_stl::detail::type_identity<T&&>;
    template<typename T> auto try_add_rvalue_reference(...)
      -> ::tiny_stl::detail::type_identity<T>;
  } // namespace tiny_stl::detail

  template<typename T> struct add_lvalue_reference
    : decltype(detail::try_add_lvalue_reference<T>(0)) {};

  // If T names a referenceable type then the member typedef type names
  // T&&; otherwise, type names T;
  template<typename T> struct add_rvalue_reference
    : decltype(detail::try_add_rvalue_reference<T>(0)) {};

  template<typename T>
    using remove_reference_t     = typename remove_reference<T>::type;
  template<typename T>
    using add_lvalue_reference_t = typename add_lvalue_reference<T>::type;
  template<typename T>
    using add_rvalue_reference_t = typename add_rvalue_reference<T>::type;

  template<typename T> struct is_copy_constructible
    : is_constructible<T, add_lvalue_reference_t<add_const_t<T>>> {};
  template<typename T> struct is_trivially_copy_constructible
    : is_trivially_constructible<T, add_lvalue_reference_t<add_const_t<T>>> {};
  template<typename T> struct is_nothrow_copy_constructible
    : is_nothrow_constructible<T, add_lvalue_reference_t<add_const_t<T>>> {};

  template<typename T> struct is_move_constructible
    : is_constructible<T, add_rvalue_reference_t<T>> {};
  template<typename T> struct is_trivially_move_constructible
    : is_trivially_constructible<T, add_rvalue_reference_t<T>> {};
  template<typename T> struct is_nothrow_move_constructible
    : is_nothrow_constructible<T, add_rvalue_reference_t<T>> {};

  template<typename T> struct is_copy_assignable
    : is_assignable<add_lvalue_reference_t<T>,
                    add_lvalue_reference_t<add_const_t<T>>> {};
  template<typename T> struct is_trivially_copy_assignable
    : is_trivially_assignable<add_lvalue_reference_t<T>,
                              add_lvalue_reference_t<add_const_t<T>>> {};
  template<typename T> struct is_nothrow_copy_assignable
    : is_nothrow_assignable<add_lvalue_reference_t<T>,
                            add_lvalue_reference_t<add_const_t<T>>> {};

  template<typename T> struct is_move_assignable
    : is_assignable<add_lvalue_reference_t<T>,
                    add_rvalue_reference_t<T>> {};
  template<typename T> struct is_trivially_move_assignable
    : is_trivially_assignable<add_lvalue_reference_t<T>,
                              add_rvalue_reference_t<T>> {};
  template<typename T> struct is_nothrow_move_assignable
    : is_nothrow_assignable<add_lvalue_reference_t<T>,
                            add_rvalue_reference_t<T>> {};

  template<typename T> struct is_swappable
    : is_swappable_with<add_lvalue_reference_t<T>,
                        add_lvalue_reference_t<T>> {};
  template<typename T> struct is_nothrow_swappable
    : is_nothrow_swappable_with<add_lvalue_reference_t<T>,
                                add_lvalue_reference_t<T>> {};

  // [meta.trans.sign], sign modifications

  // If T names a (possibly cv-qualified) signed integer type then the member
  // typedef type names the type T; otherwise, if T names a (possibly cv-qualified)
  // unsigned integer type then type names the corresponding signed integer type,
  // with the same cv-qualifiers as T; otherwise, type names the signed integer type
  // with smallest rank for which sizeof(T) == sizeof(type), with the same cv-
  // qualifiers as T.
  template<typename T> struct make_signed;

  namespace detail {
    template<bool B, typename T, typename F> struct conditional {
      using type = T;
    };
    template<typename T, typename F> struct conditional<false, T, F> {
      using type = F;
    };

    template<bool B, typename T, typename F>
      using conditional_t = typename ::tiny_stl::detail::conditional<B, T, F>::type;

    template<size_t> struct make_signed_helper;
    template<> struct make_signed_helper<1> {
      template<typename> using type = signed char;
    };
    template<> struct make_signed_helper<2> {
      template<typename> using type = short int;
    };
    template<> struct make_signed_helper<4> {
      template<typename T>
        using type = ::tiny_stl::detail::conditional_t<
          ::tiny_stl::detail::is_same_v<T, long int> || ::tiny_stl::detail::is_same_v<T, unsigned long int>,
          long int, int>;
    };
    template<> struct make_signed_helper<8> {
      template<typename T> using type = long long int;
    };

    template<typename T>
      using make_signed_helper_t = typename ::tiny_stl::detail::make_signed_helper<sizeof(T)>::template type<T>;

    template<typename T> struct is_nonbool_integral_helper
      : ::tiny_stl::true_type {};
    template<> struct is_nonbool_integral_helper<bool>
      : ::tiny_stl::false_type {};

    template<typename T> struct is_nonbool_integral
      : ::tiny_stl::detail::is_nonbool_integral_helper<::tiny_stl::detail::remove_cv_t<T>> {};
  } // namespace tiny_stl::detail

  template<typename T> struct make_signed {
    static_assert(detail::is_nonbool_integral<T>::value || is_enum<T>::value,
      "Mandates: T is an integral or enumeraion type other than cv bool");
    using type = typename detail::remove_cv<T>::template apply<detail::make_signed_helper_t>;
  };

  // If T names a (possibly cv-qualified) unsigned integer type then the member
  // typedef type names the type T; otherwise, if T names a (possibly cv-qualified)
  // signed integer type then type names the corresponding signed integer type,
  // with the same cv-qualifiers as T; otherwise, type names the signed integer type
  // with smallest rank for which sizeof(T) == sizeof(type), with the same cv-
  // qualifiers as T.
  template<typename T> struct make_unsigned;

  namespace detail {
    template<size_t> struct make_unsigned_helper;
    template<> struct make_unsigned_helper<1> {
      template<typename> using type = unsigned char;
    };
    template<> struct make_unsigned_helper<2> {
      template<typename> using type = unsigned short int;
    };
    template<> struct make_unsigned_helper<4> {
      template<typename T>
        using type = ::tiny_stl::detail::conditional_t<
          ::tiny_stl::detail::is_same_v<T, long int> || ::tiny_stl::detail::is_same_v<T, unsigned long int>,
          unsigned long int, unsigned int>;
    };
    template<> struct make_unsigned_helper<8> {
      template<typename T> using type = unsigned long long int;
    };

    template<typename T>
      using make_unsigned_helper_t = typename ::tiny_stl::detail::make_unsigned_helper<sizeof(T)>::template type<T>;
  } // namespace tiny_stl::detail

  template<typename T> struct make_unsigned {
    static_assert(detail::is_nonbool_integral<T>::value || is_enum<T>::value,
      "Mandates: T is an integral or enumeraion type other than cv bool");
    using type = typename detail::remove_cv<T>::template apply<detail::make_unsigned_helper_t>;
  };

  template<typename T>
    using make_signed_t   = typename make_signed<T>::type;
  template<typename T>
    using make_unsigned_t = typename make_unsigned<T>::type;

  // [meta.trans.arr], array modifications

  // If T names a type "array of U", the member typedef type shall be U,
  // otherwise T.
  template<typename T> struct remove_extent                 { using type = T; };
  template<typename T> struct remove_extent<T[]>            { using type = T; };
  template<typename T, size_t N> struct remove_extent<T[N]> { using type = T; };

  // If T is "multi-dimensional array of U", the resulting member typedef type
  // is U, otherwise T.
  template<typename T> struct remove_all_extents {
    using type = T;
  };
  template<typename T> struct remove_all_extents<T[]> {
    using type = typename remove_all_extents<T>::type;
  };
  template<typename T, size_t N> struct remove_all_extents<T[N]> {
    using type = typename remove_all_extents<T>::type;
  };

  template<typename T>
    using remove_extent_t      = typename remove_extent<T>::type;
  template<typename T>
    using remove_all_extents_t = typename remove_all_extents<T>::type;

  // [meta.trans.ptr], pointer modifications

  // If T has type "(possibly cv-qualified) pointer to T1" then the member typedef
  // type names T1; otherwise, it names T.
  template<typename T> struct remove_pointer;

  namespace detail {
    template<typename T> struct remove_pointer_helper     { using type = T; };
    template<typename T> struct remove_pointer_helper<T*> { using type = T; };
  } // namespace tiny_stl::detail

  template<typename T> struct remove_pointer
    : detail::remove_pointer_helper<remove_cv_t<T>> {};

  // If T names a referenceable type or a cv void type then the member typedef
  // type names the same type as remove_reference_t<T>*; otherwise, type
  // names T.
  template<typename T> struct add_pointer;

  namespace detail {
    template<typename T> auto try_add_pointer(int)
      -> ::tiny_stl::detail::type_identity<typename ::tiny_stl::remove_reference<T>::type*>;
    template<typename T> auto try_add_pointer(...)
      -> ::tiny_stl::detail::type_identity<T>;
  } // namespace tiny_stl::detail

  template<typename T> struct add_pointer
    : decltype(detail::try_add_pointer<T>(0)) {};

  template<typename T>
    using remove_pointer_t = typename remove_pointer<T>::type;
  template<typename T>
    using add_pointer_t    = typename add_pointer<T>::type;

  // [meta.trans.other], other transformations

  // The member typedef type names the type T.
  template<typename T> struct type_identity { using type = T; };

  // TODO
  // The value of default-alignment shall be the most stringent alignment
  // requirements for any object type whose size is no greater than Len
  // ([basic.types]). The member typedef type shall be a trivial standard-
  // layout type suitable for use as unitialized storage for any object
  // whose size is at most Len and whose alignment is a divisor of
  // Align.
  template<size_t Len, size_t Align = alignof(double)> struct aligned_storage {
    static_assert(Len != 0,
      "Mandates: Len is not zero. Align is equal to alignof(T) for some "
      "type T or to default-alignment.");
    struct type {
      alignas(Align) unsigned char data[Len];
    };
  };

  // TODO
  // The member typedef type shall be a trivial standard-layout type
  // suitable for use as unitialized sotrage for any object whose type is
  // listed in Types; its size shall be at least Len. The static member
  // alignment_value shall be an integral constant of type size_t
  // whose value is the strictest alignment of all types listed in Types.
  template<size_t Len, typename... Types> struct aligned_union;

  namespace detail {
    template<size_t... Vals> struct maximum;

    template<> struct maximum<>
      : ::tiny_stl::integral_constant<size_t, 0> {};
    template<size_t Val> struct maximum<Val>
      : ::tiny_stl::integral_constant<size_t, Val> {};
    template<size_t First, size_t Second, size_t... Rest> struct maximum<First, Second, Rest...>
      : ::tiny_stl::detail::maximum<(First < Second ? Second : First), Rest...>::type {};
  } // namespace tiny_stl::detail

  template<size_t Len, typename... Types> struct aligned_union {
    static constexpr size_t alignment_value = detail::maximum<alignof(Types)...>::value;
    using type = typename aligned_storage<detail::maximum<Len, sizeof(Types)...>::value,
                                          alignment_value>::type;
  };

  // The member typedef type names the same type as
  // remove_cv_t<remove_reference_t<T>>.
  template<typename T> struct remove_cvref {
    using type = remove_cv_t<remove_reference_t<T>>;
  };

  // Let U be remove_reference_t<T>. If is_array_v<U> is true, the
  // member typedef type equals remove_extent_t<U>*. If
  // is_function_v<U> is true, the member typedef type equals
  // add_pointer_t<U>. Otherwise the member typedef type equals
  // remove_cv_t<U>.
  template<typename T> struct decay {
   private:
    using U = remove_reference_t<T>;
   public:
    using type = detail::conditional_t<
      is_array<U>::value,
      remove_extent_t<U>*,
      detail::conditional_t<
        is_function<U>::value,
        add_pointer_t<U>,
        remove_cv_t<U>
      >
    >;
  };

  // If B is true, the member typedef type shall equal T; otherwise, there
  // shall be no member type.
  template<bool, typename T = void> struct enable_if {};
  template<typename T> struct enable_if<true, T> { using type = T; };

  // If B is true, the member typedef type shall equal T. If B is false,
  // the member typedef type shall equal F.
  template<bool B, typename T, typename F> struct conditional     { using type = T; };
  template<typename T, typename F> struct conditional<true, T, F> { using type = T; };

  // TODO
  // Unless this trait is specialized (as specified in Note B, below), the
  // member type is defined or ommitted as specified in Note A, below. If
  // it is omitted, there shall be no member type. Each type in the
  // template parameter pack T shall be complete, cv void, or an array of
  // unknown bound.
  template<typename... T> struct common_type;

  // TODO
  // Unless this trait is specialized (as specified in Note D, below), there
  // shall be no member type.
  template<typename T, typename U, template<typename> typename TQual, template<typename> typename UQual>
    struct basic_common_reference {};

  // TODO
  // The member typedef-name type is defined or omitted as specified
  // in Note C, below. Each type in the parameter pack T shall be
  // complete or cv void.
  template<typename... T> struct common_reference;

  // TODO
  // If T is an enumeration type, the member typedef type names the
  // underlying type of T ([dcl.enum]); otherwise, there is no member
  // type.
  template<typename T> struct underlying_type;

  // TODO
  // If the expression INVOKE(decltype<Fn>(), declval<ArgTypes>
  // ()...) is well-formed when treated as an unevaluated operand, the
  // member typedef type names the type
  // decltype(INVOKE(decltype<Fn>(), declval<ArgTypes>
  // ()...)); otherwise, there shall be no member type. Access
  // checking is performed as if in a context unrelated to Fn and
  // ArgTypes. Only the validity of the immediate context of the
  // expression is considered.
  template<typename Fn, typename... ArgTypes> struct invoke_result;

  // TODO
  // If T is a specialization reference_wrapper<X> for some type X, the
  // member typedef type of unwrap_reference<T> is X&, otherwise it
  // is T.
  template<typename T> struct unwrap_reference;

  // TODO
  // The member typedef type of unwrap_ref_decay<T> denotes the
  // type unwrap_reference_t<decay_t<T>>.
  template<typename T> struct unwrap_ref_decay;

  template<typename T>
    using type_identity_t    = typename type_identity<T>::type;
  template<size_t Len, size_t Align = alignof(double)>
    using aligned_storage_t  = typename aligned_storage<Len, Align>::type;
  template<size_t Len, typename... Types>
    using aligned_union_t    = typename aligned_union<Len, Types...>::type;
  template<typename T>
    using remove_cvref_t     = typename remove_cvref<T>::type;
  template<typename T>
    using decay_t            = typename decay<T>::type;
  template<bool B, typename T = void>
    using enable_if_t        = typename enable_if<B, T>::type;
  template<bool B, typename T, typename F>
    using contitional_t      = typename conditional<B, T, F>::type;
  template<typename... T>
    using common_type_t      = typename common_type<T...>::type;
  template<typename... T>
    using common_reference_t = typename common_reference<T...>::type;
  template<typename T>
    using underlying_type_t  = typename underlying_type<T>::type;
  template<typename Fn, typename... ArgTypes>
    using invoking_result_t  = typename invoke_result<Fn, ArgTypes...>::type;
  template<typename T>
    using unwrap_reference_t = typename unwrap_reference<T>::type;
  template<typename T>
    using unwrap_ref_decay_t = typename unwrap_ref_decay<T>::type;
  template<typename...>
    using void_t             = void;

  // [meta.logical], logical operator traits

  template<typename... B> struct conjunction
    : true_type {};
  template<typename B> struct conjunction<B>
    : B {};
  template<typename B, typename... Others> struct conjunction<B, Others...>
    : conditional<bool(B::value), conjunction<Others...>, B>::type {};

  template<typename... B> struct disjunction
    : false_type {};
  template<typename B> struct disjunction<B>
    : B {};
  template<typename B, typename... Others> struct disjunction<B, Others...>
    : conditional<bool(B::value), B, disjunction<Others...>>::type {};

  template<typename B> struct negation
    : bool_constant<!bool(B::value)> {};

  // [meta.unary.cat], primary type categories

  template<typename T>
    inline constexpr bool is_void_v = is_void<T>::value;
  template<typename T>
    inline constexpr bool is_null_pointer_v = is_null_pointer<T>::value;
  template<typename T>
    inline constexpr bool is_integral_v = is_integral<T>::value;
  template<typename T>
    inline constexpr bool is_floating_point_v = is_floating_point<T>::value;
  template<typename T>
    inline constexpr bool is_array_v = is_array<T>::value;
  template<typename T>
    inline constexpr bool is_pointer_v = is_pointer<T>::value;
  template<typename T>
    inline constexpr bool is_lvalue_reference_v = is_lvalue_reference<T>::value;
  template<typename T>
    inline constexpr bool is_rvalue_reference_v = is_rvalue_reference<T>::value;
  template<typename T>
    inline constexpr bool is_member_object_pointer_v = is_member_object_pointer<T>::value;
  template<typename T>
    inline constexpr bool is_member_function_pointer_v = is_member_function_pointer<T>::value;
  template<typename T>
    inline constexpr bool is_enum_v = is_enum<T>::value;
  template<typename T>
    inline constexpr bool is_union_v = is_union<T>::value;
  template<typename T>
    inline constexpr bool is_class_v = is_class<T>::value;
  template<typename T>
    inline constexpr bool is_function_v = is_function<T>::value;

  // [meta.unary.comp], composite type categories

  template<typename T>
    inline constexpr bool is_reference_v = is_reference<T>::value;
  template<typename T>
    inline constexpr bool is_arithmetic_v = is_arithmetic<T>::value;
  template<typename T>
    inline constexpr bool is_fundamental_v = is_fundamental<T>::value;
  template<typename T>
    inline constexpr bool is_object_v = is_object<T>::value;
  template<typename T>
    inline constexpr bool is_scalar_v = is_scalar<T>::value;
  template<typename T>
    inline constexpr bool is_compound_v = is_compound<T>::value;
  template<typename T>
    inline constexpr bool is_member_pointer_v = is_member_pointer<T>::value;

  // [meta.unary.prop], type properties

  template<typename T>
    inline constexpr bool is_const_v = is_const<T>::value;
  template<typename T>
    inline constexpr bool is_volatile_v = is_volatile<T>::value;
  template<typename T>
    inline constexpr bool is_trivial_v = is_trivial<T>::value;
  template<typename T>
    inline constexpr bool is_trivially_copyable_v = is_trivially_copyable<T>::value;
  template<typename T>
    inline constexpr bool is_standard_layout_v = is_standard_layout<T>::value;
  template<typename T>
    inline constexpr bool is_empty_v = is_empty<T>::value;
  template<typename T>
    inline constexpr bool is_polymorphic_v = is_polymorphic<T>::value;
  template<typename T>
    inline constexpr bool is_abstract_v = is_abstract<T>::value;
  template<typename T>
    inline constexpr bool is_final_v = is_final<T>::value;
  template<typename T>
    inline constexpr bool is_aggregate_v = is_aggregate<T>::value;
  template<typename T>
    inline constexpr bool is_signed_v = is_signed<T>::value;
  template<typename T>
    inline constexpr bool is_unsigned_v = is_unsigned<T>::value;
  template<typename T>
    inline constexpr bool is_bounded_array_v = is_bounded_array<T>::value;
  template<typename T>
    inline constexpr bool is_unbounded_array_v = is_unbounded_array<T>::value;
  template<typename T, typename... Args>
    inline constexpr bool is_constructible_v = is_constructible<T, Args...>::value;
  template<typename T>
    inline constexpr bool is_default_constructible_v = is_default_constructible<T>::value;
  template<typename T>
    inline constexpr bool is_copy_constructible_v = is_copy_constructible<T>::value;
  template<typename T>
    inline constexpr bool is_move_constructible_v = is_move_constructible<T>::value;
  template<typename T, typename U>
    inline constexpr bool is_assignable_v = is_assignable<T, U>::value;
  template<typename T>
    inline constexpr bool is_copy_assignable_v = is_copy_assignable<T>::value;
  template<typename T>
    inline constexpr bool is_move_assignable_v = is_move_assignable<T>::value;
  template<typename T, typename U>
    inline constexpr bool is_swappable_with_v = is_swappable_with<T, U>::value;
  template<typename T>
    inline constexpr bool is_swappable_v = is_swappable<T>::value;
  template<typename T>
    inline constexpr bool is_destructible_v = is_destructible<T>::value;
  template<typename T, typename... Args>
    inline constexpr bool is_trivially_constructible_v = is_trivially_constructible<T, Args...>::value;
  template<typename T>
    inline constexpr bool is_trivially_default_constructible_v = is_trivially_default_constructible<T>::value;
  template<typename T>
    inline constexpr bool is_trivially_copy_constructible_v = is_trivially_copy_constructible<T>::value;
  template<typename T>
    inline constexpr bool is_trivially_move_constructible_v = is_trivially_move_constructible<T>::value;
  template<typename T, typename U>
    inline constexpr bool is_trivially_assignable_v = is_trivially_assignable<T, U>::value;
  template<typename T>
    inline constexpr bool is_trivially_copy_assignable_v = is_trivially_copy_assignable<T>::value;
  template<typename T>
    inline constexpr bool is_trivially_move_assignable_v = is_trivially_move_assignable<T>::value;
  template<typename T>
    inline constexpr bool is_trivially_destructible_v = is_trivially_destructible<T>::value;
  template<typename T, typename... Args>
    inline constexpr bool is_nothrow_constructible_v = is_nothrow_constructible<T, Args...>::value;
  template<typename T>
    inline constexpr bool is_nothrow_default_constructible_v = is_nothrow_default_constructible<T>::value;
  template<typename T>
    inline constexpr bool is_nothrow_copy_constructible_v = is_nothrow_copy_constructible<T>::value;
  template<typename T>
    inline constexpr bool is_nothrow_move_constructible_v = is_nothrow_move_constructible<T>::value;
  template<typename T, typename U>
    inline constexpr bool is_nothrow_assignable_v = is_nothrow_assignable<T, U>::value;
  template<typename T>
    inline constexpr bool is_nothrow_copy_assignable_v = is_nothrow_copy_assignable<T>::value;
  template<typename T>
    inline constexpr bool is_nothrow_move_assignable_v = is_nothrow_move_assignable<T>::value;
  template<typename T, typename U>
    inline constexpr bool is_nothrow_swappable_with_v = is_nothrow_swappable_with<T, U>::value;
  template<typename T>
    inline constexpr bool is_nothrow_swappable_v = is_nothrow_swappable<T>::value;
  template<typename T>
    inline constexpr bool is_nothrow_destructible_v = is_nothrow_destructible<T>::value;
  template<typename T>
    inline constexpr bool has_virtual_destructor_v = has_virtual_destructor<T>::value;
  template<typename T>
    inline constexpr bool has_unique_object_representations_v = has_unique_object_representations<T>::value;

  // [meta.unary.prop.query], type prpperty queries

  template<typename T>
    inline constexpr size_t alignment_of_v = alignment_of<T>::value;
  template<typename T>
    inline constexpr size_t rank_v = rank<T>::value;
  template<typename T, unsigned I = 0>
    inline constexpr size_t extent_v = extent<T, I>::value;

  // [meta.rel], type relations

  template<typename T, typename U>
    inline constexpr bool is_same_v = is_same<T, U>::value;
  template<typename Base, typename Derived>
    inline constexpr bool is_base_of_v = is_base_of<Base, Derived>::value;
  template<typename From, typename To>
    inline constexpr bool is_convertible_v = is_convertible<From, To>::value;
  template<typename From, typename To>
    inline constexpr bool is_nothrow_convertible_v = is_nothrow_convertible<From, To>::value;
  template<typename T, typename U>
    inline constexpr bool is_layout_compatible_v = is_layout_compatible<T, U>::value;
  template<typename Base, typename Derived>
    inline constexpr bool is_pointer_interconvertible_base_of_v = is_pointer_interconvertible_base_of<Base, Derived>::value;
  template<typename Fn, typename... ArgTypes>
    inline constexpr bool is_invocable_v = is_invocable<Fn, ArgTypes...>::value;
  template<typename R, typename Fn, typename... ArgTypes>
    inline constexpr bool is_invocable_r_v = is_invocable_r<R, Fn, ArgTypes...>::value;
  template<typename Fn, typename... ArgTypes>
    inline constexpr bool is_nothrow_invocable_v = is_nothrow_invocable<Fn, ArgTypes...>::value;
  template<typename R, typename Fn, typename... ArgTypes>
    inline constexpr bool is_nothrow_invocable_r_v = is_nothrow_invocable_r<R, Fn, ArgTypes...>::value;

  // [meta.logical], logical operator traits

  template<typename... B>
    inline constexpr bool conjunction_v = conjunction<B...>::value;
  template<typename... B>
    inline constexpr bool disjunction_v = disjunction<B...>::value;
  template<typename B>
    inline constexpr bool negation_v = negation<B>::value;

  // [meta.member], member relationships

  // TODO
  // Mandates: S is a complete type.
  // Returns: true if and only if S is a standard-layout type, M is an object type, m is not null, and each object s of
  // type S is pointer-interconvertible ([basic.compound]) with its subobject s.*m.
  template<typename S, typename M>
    constexpr bool is_pointer_interconvertible_with_class(M S::*m) noexcept;

  // TODO
  // Mandates: S1 and S2 are complete types
  // Returns: true if and only if S1 and S2 are standard-layout types, M1 and M2 are object types, m1 and m2 are not
  // null, and m1 and m2 point to corresponding members of the common intital sequence ([class.mem]) of S1 and
  // S2.
  template<typename S1, typename S2, typename M1, typename M2>
    constexpr bool is_corresponding_member(M1 S1::*m1, M2 S2::*m2) noexcept;

  // [meta.const.eval], constant evaluation context

  // TODO
  // Returns: true if and only if evaluation of the call occurs within the evaluation of an expression or conversion
  // that is manifestly constant-evaluated ([expr.const]).
  constexpr bool is_constant_evaluated() noexcept;

} // namespace tiny_stl

#endif // TINY_STL_TYPE_TRAITS_HPP