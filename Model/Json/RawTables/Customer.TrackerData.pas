unit Customer.TrackerData;

// ----------------------------------------
// Database model for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


uses
    Generics.Collections,
    Rest.Json;

	
type


	TTrackerData = class
	private
		FId:				TArray<integer>;
		FUserAlias:			TArray<string>;
		FCuid:				TArray<LongInt>;
		FCoCode:			TArray<string>;
		FBranch:			TArray<string>;
		FCustomerName:		TArray<string>;
		FStamp:				TArray<TDateTime>;
		FSendReminder1:		TArray<integer>;
		FSendReminder2:		TArray<integer>;
		FSendReminder3:		TArray<integer>;
		FSendReminder4:		TArray<integer>;
		FSciud:				TArray<LongInt>;
		FReminderLayout:	TArray<string>;
		FPreStatement:		TArray<integer>;
		FSendFrom:			TArray<string>;
		FStatementTo:		TArray<string>;
		FReminderTo:		TArray<string>;
	public
		property Id:				TArray<integer>		read FId			    write FId;
		property UserAlias:			TArray<string>		read FUserAlias		    write FUserAlias;
		property Cuid:				TArray<LongInt>		read FCuid			    write FCuid;
		property CoCode:			TArray<string>		read FCoCode		    write FCoCode;
		property Branch:			TArray<string>		read FBranch		    write FBranch;
		property CustomerName:		TArray<string>		read FCustomerName	    write FCustomerName;
		property Stamp:				TArray<TDateTime>	read FStamp			    write FStamp;
		property SendReminder1:		TArray<integer>		read FSendReminder1	    write FSendReminder1;
		property SendReminder2:		TArray<integer>		read FSendReminder2	    write FSendReminder2;
		property SendReminder3:		TArray<integer>		read FSendReminder3	    write FSendReminder3;
		property SendReminder4:		TArray<integer>		read FSendReminder4	    write FSendReminder4;
		property Sciud:				TArray<LongInt>		read FSciud			    write FSciud;
		property ReminderLayout:	TArray<string>		read FReminderLayout	write FReminderLayout;
		property PreStatement:		TArray<integer>		read FPreStatement	    write FPreStatement;
		property SendFrom:			TArray<string>		read FSendFrom		    write FSendFrom;
		property StatementTo:		TArray<string>		read FStatementTo	    write FStatementTo;
		property ReminderTo:		TArray<string>		read FReminderTo	    write FReminderTo;
	end;

	
implementation


end.
