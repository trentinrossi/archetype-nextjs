export interface CardCrossReference {
  accountId: string;
  cardNumber: string;
  createdAt: string;
  updatedAt: string;
}

export interface CreateCardCrossReferenceRequest {
  accountId: string;
  cardNumber: string;
}

export interface PaginatedCardCrossReferences {
  content: CardCrossReference[];
  pageable: {
    pageNumber: number;
    pageSize: number;
    sort: {
      sorted: boolean;
      unsorted: boolean;
      empty: boolean;
    };
    offset: number;
    paged: boolean;
    unpaged: boolean;
  };
  totalElements: number;
  totalPages: number;
  last: boolean;
  size: number;
  number: number;
  sort: {
    sorted: boolean;
    unsorted: boolean;
    empty: boolean;
  };
  numberOfElements: number;
  first: boolean;
  empty: boolean;
}
