'use client';

import React, { createContext, useContext, useState } from 'react';
import { CommArea } from '@/types/user';

interface UserManagementContextType {
  commArea: CommArea;
  updateCommArea: (updates: Partial<CommArea>) => void;
  resetCommArea: () => void;
}

const defaultCommArea: CommArea = {
  programReenter: false,
  toProgram: '',
  fromProgram: '',
  fromTransactionId: '',
  userSelected: '',
  pageNumber: 0,
  firstUserId: '',
  lastUserId: '',
  nextPageFlag: false,
  userSelectionFlag: undefined,
  programContext: 0
};

const UserManagementContext = createContext<UserManagementContextType | undefined>(undefined);

export function UserManagementProvider({ children }: { children: React.ReactNode }) {
  const [commArea, setCommArea] = useState<CommArea>(defaultCommArea);

  const updateCommArea = (updates: Partial<CommArea>) => {
    setCommArea(prev => ({ ...prev, ...updates }));
  };

  const resetCommArea = () => {
    setCommArea(defaultCommArea);
  };

  return (
    <UserManagementContext.Provider value={{ 
      commArea, 
      updateCommArea, 
      resetCommArea 
    }}>
      {children}
    </UserManagementContext.Provider>
  );
}

export function useUserManagement() {
  const context = useContext(UserManagementContext);
  if (context === undefined) {
    throw new Error('useUserManagement must be used within a UserManagementProvider');
  }
  return context;
}