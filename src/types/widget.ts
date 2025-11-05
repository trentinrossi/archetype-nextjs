export interface Widget {
  id: string;
  name: string;
  category: string;
  price: number;
  isActive: boolean;
  createdAt: string;
  updatedAt: string;
  description?: string;
}

export interface CreateWidgetRequest {
  name: string;
  category: string;
  price: number;
  isActive?: boolean;
  description?: string;
}

export interface UpdateWidgetRequest {
  name?: string;
  category?: string;
  price?: number;
  isActive?: boolean;
  description?: string;
}

export interface ApiError {
  error: string;
  status: number;
  details?: string;
}
