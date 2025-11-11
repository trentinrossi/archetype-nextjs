export interface AdminMenuOption {
  id: number;
  optionNumber: number;
  optionName: string;
  programName: string;
  isActive: boolean;
  statusDisplay: string;
  adminUserId: string;
  optionDisplay: string;
  createdAt: string;
  updatedAt: string;
}

export interface CreateAdminMenuOptionRequest {
  optionNumber: number;
  optionName: string;
  programName: string;
  isActive: boolean;
  adminUserId: string;
}

export interface UpdateAdminMenuOptionRequest {
  optionName?: string;
  isActive?: boolean;
}
