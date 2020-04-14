unit Api.UserCompanyFields;

// -------------------------------------------------------------
// JSON model for REST. Can be referenced by anyone. Cannot hold
// references to View or Logic. Cannot have any implementation
// apart from fields/class initialization/release.
// Note: Do not use TList in model, instead use TArray<T> and
// use TList.ToArray to pass prepared data to the target model.
// -------------------------------------------------------------

interface


type


	TUserCompanyFields = class
	strict private
        var FCompany:     string;
        var FIsSelected:  boolean;
	public
        const _Company    = 'Company';
        const _IsSelected = 'IsSelected';
        property Companies:   string  read FCompany    write FCompany;
        property IsSelected:  boolean read FIsSelected write FIsSelected;
	end;


implementation


end.
