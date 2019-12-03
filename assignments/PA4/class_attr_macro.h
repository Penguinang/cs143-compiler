#ifndef CLASS_ATTR_MACRO
#define CLASS_ATTR_MACRO

#define GETTER(att) \
    inline auto get_##att() { return this->att; }
#define SETTER(att) \
    inline void set_##att(decltype(this->att) const &val) { this->att = val; }


#endif // !CLASS_ATTR_MACRO