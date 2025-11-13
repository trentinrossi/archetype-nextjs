export interface MenuOption {
  id: number;
  optionNumber: number;
  optionName: string;
  programName: string;
  userTypeRequired: string;
  optionCount: number;
  isActive: boolean;
  displayOrder: number;
  isAdminOnly: boolean;
  isUserAccessible: boolean;
  isComingSoon: boolean;
  accessLevelDisplay: string;
  statusDisplay: string;
  createdAt: string;
  updatedAt: string;
}

export interface CreateMenuOptionRequest {
  optionNumber: number;
  optionName: string;
  programName: string;
  userTypeRequired: string;
  optionCount: number;
  isActive: boolean;
  displayOrder: number;
}

export interface UpdateMenuOptionRequest {
  optionNumber?: number;
  optionName?: string;
  programName?: string;
  userTypeRequired?: string;
  optionCount?: number;
  isActive?: boolean;
  displayOrder?: number;
}

export interface AdminMenuOption {
  id: number;
  optionNumber: number;
  optionName: string;
  programName: string;
  isActive: boolean;
  displayOrder: number;
  isComingSoon: boolean;
  statusDisplay: string;
  createdAt: string;
  updatedAt: string;
}
