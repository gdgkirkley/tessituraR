
Season <- R6::R6Class(
  classname = "Season",
  public = list(
    ConfirmationNoticeFormat = NULL,
    CreatedDateTime = NULL,
    CreateLocation = NULL,
    CreatedBy = NULL,
    DefaultIndicator = NULL,
    Description = NULL,
    DisplayInSeasonOverview = NULL,
    EndDateTime = NULL,
    FYear = NULL,
    Id = NULL,
    Inactive = NULL,
    UpdatedDateTime = NULL,
    UpdatedBy = NULL,
    RenewalNoticeFormat = NULL,
    StartDateTime = NULL,
    SubscriptionFund1 = NULL,
    SubscriptionFund2 = NULL,
    Type = NULL,
    YearlySeason = NULL,
    ControlGroup = NULL,

    initialize = function(
      ConfirmationNoticeFormat = NULL,
      CreatedDateTime = NULL,
      CreateLocation = NULL,
      CreatedBy = NULL,
      DefaultIndicator = NULL,
      Description = NULL,
      DisplayInSeasonOverview = NULL,
      EndDateTime = NULL,
      FYear = NULL,
      Id = NULL,
      Inactive = NULL,
      UpdatedDateTime = NULL,
      UpdatedBy = NULL,
      RenewalNoticeFormat = NULL,
      StartDateTime = NULL,
      SubscriptionFund1 = NULL,
      SubscriptionFund2 = NULL,
      Type = NULL,
      YearlySeason = NULL,
      ControlGroup = NULL
    ) {
      if(!is.null(ConfirmationNoticeFormat)) {
        self$ConfirmationNoticeFormat <- ConfirmationNoticeFormat
      }

      if(!is.null(CreatedDateTime)) {
        self$CreatedDateTime <- CreatedDateTime
      }

      if(!is.null(CreateLocation)) {
        self$CreateLocation <- CreateLocation
      }

      if(!is.null(CreatedBy)) {
        self$CreatedBy <- CreatedBy
      }

      if(!is.null(DefaultIndicator)) {
        self$DefaultIndicator <- DefaultIndicator
      }

      if(!is.null(Description)) {
        self$Description <- Description
      }

      if(!is.null(DisplayInSeasonOverview)) {
        self$DisplayInSeasonOverview <- DisplayInSeasonOverview
      }

      if(!is.null(EndDateTime)) {
        self$EndDateTime <- EndDateTime
      }

      if(!is.null(FYear)) {
        self$FYear <- FYear
      }

      if(!is.null(Id)) {
        self$Id <- Id
      }

      if(!is.null(Inactive)) {
        self$Inactive <- Inactive
      }

      if(!is.null(UpdatedDateTime)) {
        self$UpdatedDateTime <- UpdatedDateTime
      }

      if(!is.null(UpdatedBy)) {
        self$UpdatedBy <- UpdatedBy
      }

      if(!is.null(RenewalNoticeFormat)) {
        self$RenewalNoticeFormat <- RenewalNoticeFormat
      }

      if(!is.null(StartDateTime)) {
        self$StartDateTime <- StartDateTime
      }

      if(!is.null(SubscriptionFund1)) {
        self$SubscriptionFund1 <- SubscriptionFund1
      }

      if(!is.null(SubscriptionFund2)) {
        self$SubscriptionFund2 <- SubscriptionFund2
      }

      if(!is.null(Type)) {
        self$Type <- Type
      }

      if(!is.null(YearlySeason)) {
        self$YearlySeason <- YearlySeason
      }

      if(!is.null(ControlGroup)) {
        self$ControlGroup <- ControlGroup
      }
    },

    toJSON = function() {
      SeasonObj <- list()

      if(!is.null(self$ConfirmationNoticeFormat)) {
        SeasonObj[["ConfirmationNoticeFormat"]] <- self$ConfirmationNoticeFormat
      }

      if(!is.null(self$CreatedDateTime)) {
        SeasonObj[["CreatedDateTime"]] <- self$CreatedDateTime
      }

      if(!is.null(self$CreateLocation)) {
        SeasonObj[["CreateLocation"]] <- self$CreateLocation
      }

      if(!is.null(self$CreatedBy)) {
        SeasonObj[["CreatedBy"]] <- self$CreatedBy
      }

      if(!is.null(self$DefaultIndicator)) {
        SeasonObj[["DefaultIndicator"]] <- self$DefaultIndicator
      }

      if(!is.null(self$Description)) {
        SeasonObj[["Description"]] <- self$Description
      }

      if(!is.null(self$DisplayInSeasonOverview)) {
        SeasonObj[["DisplayInSeasonOverview"]] <- self$DisplayInSeasonOverview
      }

      if(!is.null(self$EndDateTime)) {
        SeasonObj[["EndDateTime"]] <- self$EndDateTime
      }

      if(!is.null(self$FYear)) {
        SeasonObj[["FYear"]] <- self$FYear
      }

      if(!is.null(self$Id)) {
        SeasonObj[["Id"]] <- self$Id
      }

      if(!is.null(self$Inactive)) {
        SeasonObj[["Inactive"]] <- self$Inactive
      }

      if(!is.null(self$UpdatedDateTime)) {
        SeasonObj[["UpdatedDateTime"]] <- self$UpdatedDateTime
      }

      if(!is.null(self$UpdatedBy)) {
        SeasonObj[["UpdatedBy"]] <- self$UpdatedBy
      }

      if(!is.null(self$RenewalNoticeFormat)) {
        SeasonObj[["RenewalNoticeFormat"]] <- self$RenewalNoticeFormat
      }

      if(!is.null(self$StartDateTime)) {
        SeasonObj[["StartDateTime"]] <- self$StartDateTime
      }

      if(!is.null(self$SubscriptionFund1)) {
        SeasonObj[["SubscriptionFund1"]] <- self$SubscriptionFund1
      }

      if(!is.null(self$StartDateTime)) {
        SeasonObj[["StartDateTime"]] <- self$StartDateTime
      }

      if(!is.null(self$SubscriptionFund2)) {
        SeasonObj[["SubscriptionFund2"]] <- self$SubscriptionFund2
      }

      if(!is.null(self$Type)) {
        SeasonObj[["Type"]] <- self$Type
      }

      if(!is.null(self$YearlySeason)) {
        SeasonObj[["YearlySeason"]] <- self$YearlySeason
      }

      if(!is.null(self$ControlGroup)) {
        SeasonObj[["ControlGroup"]] <- self$ControlGroup
      }

      SeasonObj
    },

    fromJSON = function(SeasonJSON) {
      SeasonObj = jsonlite::fromJSON(SeasonJSON)

      if(!is.null(SeasonObj$ConfirmationNoticeFormat)) {
        self$ConfirmationNoticeFormat <- SeasonObj$ConfirmationNoticeFormat
      }

      if(!is.null(SeasonObj$CreatedDateTime)) {
        self$CreatedDateTime <- SeasonObj$CreatedDateTime
      }

      if(!is.null(self$CreateLocation)) {
        SeasonObj[["CreateLocation"]] <- self$CreateLocation
      }

      if(!is.null(SeasonObj$CreatedBy)) {
        self$CreatedBy <- SeasonObj$CreatedBy
      }

      if(!is.null(SeasonObj$DefaultIndicator)) {
        self$DefaultIndicator <- SeasonObj$DefaultIndicator
      }

      if(!is.null(SeasonObj$Description)) {
        self$Description <- SeasonObj$Description
      }

      if(!is.null(SeasonObj$DisplayInSeasonOverview)) {
        self$DisplayInSeasonOverview <- SeasonObj$DisplayInSeasonOverview
      }

      if(!is.null(SeasonObj$EndDateTime)) {
        self$EndDateTime <- SeasonObj$EndDateTime
      }

      if(!is.null(SeasonObj$FYear)) {
        self$FYear <- SeasonObj$FYear
      }

      if(!is.null(SeasonObj$Id)) {
        self$Id <- SeasonObj$Id
      }

      if(!is.null(SeasonObj$Inactive)) {
        self$Inactive <- SeasonObj$Inactive
      }

      if(!is.null(SeasonObj$UpdatedDateTime)) {
        self$UpdatedDateTime <- SeasonObj$UpdatedDateTime
      }

      if(!is.null(SeasonObj$UpdatedBy)) {
        self$UpdatedBy <- SeasonObj$UpdatedBy
      }

      if(!is.null(SeasonObj$RenewalNoticeFormat)) {
        self$RenewalNoticeFormat <- SeasonObj$RenewalNoticeFormat
      }

      if(!is.null(SeasonObj$StartDateTime)) {
        self$StartDateTime <- SeasonObj$StartDateTime
      }

      if(!is.null(SeasonObj$SubscriptionFund1)) {
        self$SubscriptionFund1 <- SeasonObj$SubscriptionFund1
      }

      if(!is.null(SeasonObj$StartDateTime)) {
        self$StartDateTime <- SeasonObj$StartDateTime
      }

      if(!is.null(SeasonObj$SubscriptionFund2)) {
        self$SubscriptionFund2 <- SeasonObj$SubscriptionFund2
      }

      if(!is.null(SeasonObj$Type)) {
        self$Type <- SeasonObj$Type
      }

      if(!is.null(SeasonObj$YearlySeason)) {
        self$YearlySeason <- SeasonObj$YearlySeason
      }

      if(!is.null(SeasonObj$ControlGroup)) {
        self$ControlGroup <- SeasonObj$ControlGroup
      }
    }
  )
)
