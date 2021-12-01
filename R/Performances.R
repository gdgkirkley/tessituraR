#' Tessitura Performances
#'
#'
Performances <- R6::R6Class(
  classname = "Performance",
  public = list(
    AvailSaleIndicator = NULL,
    BestSeatMap = NULL,
    BudgetAmount = NULL,
    Campaign = NULL,
    Code = NULL,
    CreatedBy = NULL,
    CreatedDateTime = NULL,
    CreateLocation = NULL,
    Date = NULL,
    DefaultEndSaleDateTime = NULL,
    DefaultStartSaleDateTime = NULL,
    Description = NULL,
    DoorsClose = NULL,
    DoorsOpen = NULL,
    Duration = NULL,
    EditIndicator = NULL,
    Facility = NULL,
    Id = NULL,
    ProductionSeason = NULL,
    PublishClientEndDate = NULL,
    PublishClientStartDate = NULL,
    PublishWebApiEndDate = NULL,
    PublishWebApiStartDate = NULL,
    RankType = NULL,
    SalesNotes = NULL,
    SalesNotesRequired = NULL,
    Season = NULL,
    ShortName = NULL,
    Status = NULL,
    Text1 = NULL,
    Text2 = NULL,
    Text3 = NULL,
    Text4 = NULL,
    TimeSlot = NULL,
    TvIndicator = NULL,
    Type = NULL,
    UpdatedBy = NULL,
    UpdatedDateTime = NULL,
    ZoneMap = NULL,

    initialize <- function(AvailSaleIndicator = NULL,
                          BestSeatMap = NULL,
                          BudgetAmount = NULL,
                          Campaign = NULL,
                          Code = NULL,
                          CreatedBy = NULL,
                          CreatedDateTime = NULL,
                          CreateLocation = NULL,
                          Date = NULL,
                          DefaultEndSaleDateTime = NULL,
                          DefaultStartSaleDateTime = NULL,
                          Description = NULL,
                          DoorsClose = NULL,
                          DoorsOpen = NULL,
                          Duration = NULL,
                          EditIndicator = NULL,
                          Facility = NULL,
                          Id = NULL,
                          ProductionSeason = NULL,
                          PublishClientEndDate = NULL,
                          PublishClientStartDate = NULL,
                          PublishWebApiEndDate = NULL,
                          PublishWebApiStartDate = NULL,
                          RankType = NULL,
                          SalesNotes = NULL,
                          SalesNotesRequired = NULL,
                          Season = NULL,
                          ShortName = NULL,
                          Status = NULL,
                          Text1 = NULL,
                          Text2 = NULL,
                          Text3 = NULL,
                          Text4 = NULL,
                          TimeSlot = NULL,
                          TvIndicator = NULL,
                          Type = NULL,
                          UpdatedBy = NULL,
                          UpdatedDateTime = NULL,
                          ZoneMap = NULL) {
      if (!is.null(AvailSaleIndicator)) {
        self$AvailSaleIndicator <- AvailSaleIndicator
      }

      if (!is.null(BestSeatMap)) {
        self$BestSeatMap <- BestSeatMap
      }

      if (!is.null(BudgetAmount)) {
        self$BudgetAmount <- BudgetAmount
      }

      if (!is.null(Campaign)) {
        self$Campaign <- Campaign
      }

      if (!is.null(Code)) {
        self$Code <- Code
      }

      if (!is.null(CreatedBy)) {
        self$CreatedBy <- CreatedBy
      }

      if (!is.null(CreatedDateTime)) {
        self$CreatedDateTime <- CreatedDateTime
      }

      if (!is.null(CreateLocation)) {
        self$CreateLocation <- CreateLocation
      }

      if (!is.null(Date)) {
        self$Date <- Date
      }

      if (!is.null(DefaultEndSaleDateTime)) {
        self$DefaultEndSaleDateTime <- DefaultEndSaleDateTime
      }

      if (!is.null(Description)) {
        self$Description <- Description
      }

      if (!is.null(DoorsClose)) {
        self$DoorsClose <- DoorsClose
      }

      if (!is.null(DoorsOpen)) {
        self$DoorsOpen <- DoorsOpen
      }

      if (!is.null(Duration)) {
        self$Duration <- Duration
      }

      if (!is.null(EditIndicator)) {
        self$EditIndicator <- EditIndicator
      }

      if (!is.null(Facility)) {
        self$Facility <- Facility
      }

      if (!is.null(Id)) {
        self$Id <- Id
      }

      if (!is.null(ProductionSeason)) {
        self$ProductionSeason <- ProductionSeason
      }

      if (!is.null(PublishClientEndDate)) {
        self$PublishClientEndDate <- PublishClientEndDate
      }

      if (!is.null(PublishClientStartDate)) {
        self$PublishClientStartDate <- PublishClientStartDate
      }

      if (!is.null(PublishWebApiEndDate)) {
        self$PublishWebApiEndDate <- PublishWebApiEndDate
      }

      if (!is.null(PublishWebApiStartDate)) {
        self$PublishWebApiStartDate <- PublishWebApiStartDate
      }

      if (!is.null(PublishClientStartDate)) {
        self$PublishClientStartDate <- PublishClientStartDate
      }

      if (!is.null(RankType)) {
        self$RankType <- RankType
      }

      if (!is.null(SalesNotes)) {
        self$SalesNotes <- SalesNotes
      }

      if (!is.null(SalesNotesRequired)) {
        self$SalesNotesRequired <- SalesNotesRequired
      }

      if (!is.null(Season)) {
        self$Season <- Season
      }

      if (!is.null(ShortName)) {
        self$ShortName <- ShortName
      }

      if (!is.null(Status)) {
        self$Status <- Status
      }

      if (!is.null(Text1)) {
        self$Text1 <- Text1
      }

      if (!is.null(Text2)) {
        self$Text2 <- Text2
      }

      if (!is.null(Text3)) {
        self$Text3 <- Text3
      }

      if (!is.null(Text4)) {
        self$Text4 <- Text4
      }

      if (!is.null(TimeSlot)) {
        self$TimeSlot <- TimeSlot
      }

      if (!is.null(TvIndicator)) {
        self$TvIndicator <- TvIndicator
      }

      if (!is.null(Type)) {
        self$Type <- Type
      }

      if (!is.null(UpdatedBy)) {
        self$UpdatedBy <- UpdatedBy
      }

      if (!is.null(UpdatedDateTime)) {
        self$UpdatedDateTime <- UpdatedDateTime
      }

      if (!is.null(ZoneMap)) {
        self$ZoneMap <- ZoneMap
      }
    },

    toJSON <- function() {
      PerformanceObject <- list()
      if (!is.null(self$AvailSaleIndicator)) {
        PerformanceObject[['AvailSaleIndicator']] <- AvailSaleIndicator
      }

      if (!is.null(self$BestSeatMap)) {
        PerformanceObject[['BestSeatMap']] <- BestSeatMap
      }

      if (!is.null(self$BudgetAmount)) {
        PerformanceObject[['BudgetAmount']] <- BudgetAmount
      }

      if (!is.null(self$Campaign)) {
        PerformanceObject[['Campaign']] <- Campaign
      }

      if (!is.null(self$Code)) {
        PerformanceObject[['Code']] <- Code
      }

      if (!is.null(self$CreatedBy)) {
        PerformanceObject[['CreatedBy']] <- CreatedBy
      }

      if (!is.null(self$CreatedDateTime)) {
        PerformanceObject[['CreatedDateTime']] <- CreatedDateTime
      }

      if (!is.null(self$CreateLocation)) {
        PerformanceObject[['CreateLocation']] <- CreateLocation
      }

      if (!is.null(self$Date)) {
        PerformanceObject[['Date']] <- Date
      }

      if (!is.null(self$DefaultEndSaleDateTime)) {
        PerformanceObject[['DefaultEndSaleDateTime']] <- DefaultEndSaleDateTime
      }

      if (!is.null(self$Description)) {
        PerformanceObject[['Description']] <- Description
      }

      if (!is.null(self$DoorsClose)) {
        PerformanceObject[['DoorsClose']] <- DoorsClose
      }

      if (!is.null(self$DoorsOpen)) {
        PerformanceObject[['DoorsOpen']] <- DoorsOpen
      }

      if (!is.null(self$Duration)) {
        PerformanceObject[['Duration']] <- Duration
      }

      if (!is.null(self$EditIndicator)) {
        PerformanceObject[['EditIndicator']] <- EditIndicator
      }

      if (!is.null(self$Facility)) {
        PerformanceObject[['Facility']] <- Facility
      }

      if (!is.null(self$Id)) {
        PerformanceObject[['Id']] <- Id
      }

      if (!is.null(self$ProductionSeason)) {
        PerformanceObject[['ProductionSeason']] <- ProductionSeason
      }

      if (!is.null(self$PublishClientEndDate)) {
        PerformanceObject[['PublishClientEndDate']] <- PublishClientEndDate
      }

      if (!is.null(self$PublishClientStartDate)) {
        PerformanceObject[['PublishClientStartDate']] <- PublishClientStartDate
      }

      if (!is.null(self$PublishWebApiEndDate)) {
        PerformanceObject[['PublishWebApiEndDate']] <- PublishWebApiEndDate
      }

      if (!is.null(self$PublishWebApiStartDate)) {
        PerformanceObject[['PublishWebApiStartDate']] <- PublishWebApiStartDate
      }

      if (!is.null(self$PublishClientStartDate)) {
        PerformanceObject[['PublishClientStartDate']] <- PublishClientStartDate
      }

      if (!is.null(self$RankType)) {
        PerformanceObject[['RankType']] <- RankType
      }

      if (!is.null(self$SalesNotes)) {
        PerformanceObject[['SalesNotes']] <- SalesNotes
      }

      if (!is.null(self$SalesNotesRequired)) {
        PerformanceObject[['SalesNotesRequired']] <- SalesNotesRequired
      }

      if (!is.null(self$Season)) {
        PerformanceObject[['Season']] <- Season
      }

      if (!is.null(self$ShortName)) {
        PerformanceObject[['ShortName']] <- ShortName
      }

      if (!is.null(self$Status)) {
        PerformanceObject[['Status']] <- Status
      }

      if (!is.null(self$Text1)) {
        PerformanceObject[['Text1']] <- Text1
      }

      if (!is.null(self$Text2)) {
        PerformanceObject[['Text2']] <- Text2
      }

      if (!is.null(self$Text3)) {
        PerformanceObject[['Text3']] <- Text3
      }

      if (!is.null(self$Text4)) {
        PerformanceObject[['Text4']] <- Text4
      }

      if (!is.null(self$TimeSlot)) {
        PerformanceObject[['TimeSlot']] <- TimeSlot
      }

      if (!is.null(self$TvIndicator)) {
        PerformanceObject[['TvIndicator']] <- TvIndicator
      }

      if (!is.null(self$Type)) {
        PerformanceObject[['Type']] <- Type
      }

      if (!is.null(self$UpdatedBy)) {
        PerformanceObject[['UpdatedBy']] <- UpdatedBy
      }

      if (!is.null(self$UpdatedDateTime)) {
        PerformanceObject[['UpdatedDateTime']] <- UpdatedDateTime
      }

      if (!is.null(self$ZoneMap)) {
        PerformanceObject[['ZoneMap']] <- ZoneMap
      }

      PerformanceObject
    }
  )
)
