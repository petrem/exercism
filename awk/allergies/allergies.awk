BEGIN {
    FS=","
    allergens["eggs"] = 0x01;
    allergens["peanuts"] = 0x02;
    allergens["shellfish"] = 0x04;
    allergens["strawberries"] = 0x08;
    allergens["tomatoes"] = 0x10;
    allergens["chocolate"] = 0x20;
    allergens["pollen"] = 0x40;
    allergens["cats"] = 0x80;
}
{
    switch($2) {
    case "allergic_to":
        if (is_allergic_to(and($1, 0xff), $3)) {
            print "true";
        } else {
            print "false";
        }
        break
    case "list":
        printf list_allergies(and($1, 0xff));
        break
    default:
        print "error: unknown function", $2
        break
    }
}

function is_allergic_to(score, allergen) {
    return and(allergens[allergen], score);
}

function list_allergies(score) {
    comma_hack = ""
    PROCINFO["sorted_in"] = "@val_num_asc"
    for (allergen in allergens) {
        if (is_allergic_to(score, allergen)) {
                printf comma_hack allergen
                comma_hack = ","
        }
    }
}
