'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui/Button';
import { 
  UserSecurityDTO, 
  UserListResponse, 
  UserListRequest,
  USER_TYPE_LABELS,
  DEFAULT_PAGE_SIZE 
} from '@/types/user';
import { userService } from '@/services/userService';
import { useUserAuth } from '@/contexts/UserAuthContext';

interface UserListProps {
  onUserSelect?: (userId: string, action: 'update' | 'delete') => void;
  onExit?: () => void;
  className?: string;
}

interface UserListState {
  users: UserSecurityDTO[];
  totalCount: number;
  currentPage: number;
  totalPages: number;
  pageSize: number;
  filter: string;
  selectedUserId: string;
  selectedAction: string;
  isLoading: boolean;
  error: string;
  message: string;
  isSubmitting: boolean;
}

const UserList: React.FC<UserListProps> = ({
  onUserSelect,
  onExit,
  className = '',
}) => {
  const router = useRouter();
  const { user: currentUser } = useUserAuth();
  const [state, setState] = useState<UserListState>({
    users: [],
    totalCount: 0,
    currentPage: 0,
    totalPages: 0,
    pageSize: DEFAULT_PAGE_SIZE,
    filter: '',
    selectedUserId: '',
    selectedAction: '',
    isLoading: true,
    error: '',
    message: '',
    isSubmitting: false,
  });

  const updateState = useCallback((updates: Partial<UserListState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  const loadUsers = useCallback(async (params: Partial<UserListRequest> = {}) => {
    updateState({ isLoading: true, error: '', message: '' });

    try {
      const requestParams: UserListRequest = {
        page: state.currentPage,
        pageSize: state.pageSize,
        filter: state.filter || undefined,
        sortBy: 'userId',
        sortOrder: 'asc',
        ...params,
      };

      const response: UserListResponse = await userService.getUsers(requestParams);

      updateState({
        users: response.users,
        totalCount: response.totalCount,
        currentPage: response.currentPage,
        totalPages: response.totalPages,
        pageSize: response.pageSize,
        isLoading: false,
      });
    } catch (error: any) {
      updateState({
        error: error.message || 'Unable to lookup User...',
        isLoading: false,
      });
    }
  }, [state.currentPage, state.pageSize, state.filter, updateState]);

  const handleFilterChange = useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    const value = event.target.value.toUpperCase().slice(0, 8);
    updateState({ filter: value });
  }, [updateState]);

  const handleFilterSubmit = useCallback(async () => {
    if (state.isSubmitting) return;

    updateState({ currentPage: 0, isSubmitting: true });
    await loadUsers({ page: 0, filter: state.filter || undefined });
    updateState({ isSubmitting: false });
  }, [state.filter, state.isSubmitting, loadUsers, updateState]);

  const handlePreviousPage = useCallback(async () => {
    if (state.currentPage <= 0 || state.isSubmitting) {
      updateState({
        message: 'You are already at the top of the page...',
      });
      return;
    }

    const newPage = state.currentPage - 1;
    updateState({ currentPage: newPage, isSubmitting: true });
    await loadUsers({ page: newPage });
    updateState({ isSubmitting: false });
  }, [state.currentPage, state.isSubmitting, loadUsers, updateState]);

  const handleNextPage = useCallback(async () => {
    if (state.currentPage >= state.totalPages - 1 || state.isSubmitting) {
      updateState({
        message: 'You are already at the bottom of the page...',
      });
      return;
    }

    const newPage = state.currentPage + 1;
    updateState({ currentPage: newPage, isSubmitting: true });
    await loadUsers({ page: newPage });
    updateState({ isSubmitting: false });
  }, [state.currentPage, state.totalPages, state.isSubmitting, loadUsers, updateState]);

  const handleUserAction = useCallback((userId: string, action: string) => {
    if (action.toUpperCase() === 'U') {
      if (onUserSelect) {
        onUserSelect(userId, 'update');
      } else {
        router.push(`/users/update/${userId}`);
      }
    } else if (action.toUpperCase() === 'D') {
      if (onUserSelect) {
        onUserSelect(userId, 'delete');
      } else {
        router.push(`/users/delete/${userId}`);
      }
    } else {
      updateState({
        error: 'Invalid selection. Valid values are U and D',
      });
    }
  }, [onUserSelect, router, updateState]);

  const handleExit = useCallback(async () => {
    if (state.isSubmitting) return;

    if (onExit) {
      onExit();
    } else {
      router.push('/dashboard');
    }
  }, [state.isSubmitting, onExit, router]);

  const handleKeyDown = useCallback(async (event: KeyboardEvent) => {
    if (state.isSubmitting) return;

    switch (event.key) {
      case 'F7':
        event.preventDefault();
        await handlePreviousPage();
        break;
      case 'F8':
        event.preventDefault();
        await handleNextPage();
        break;
      case 'F3':
        event.preventDefault();
        await handleExit();
        break;
      case 'Enter':
        event.preventDefault();
        await handleFilterSubmit();
        break;
      default:
        if (event.key.startsWith('F') && !['F3', 'F7', 'F8'].includes(event.key)) {
          event.preventDefault();
          updateState({
            error: 'INVALID KEY PRESSED',
          });
        }
        break;
    }
  }, [
    state.isSubmitting,
    handlePreviousPage,
    handleNextPage,
    handleExit,
    handleFilterSubmit,
    updateState,
  ]);

  useEffect(() => {
    loadUsers();
  }, []);

  useEffect(() => {
    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [handleKeyDown]);

  return (
    <div className={`min-h-screen bg-blue-900 p-4 ${className}`}>
      <div className="bg-white rounded-lg shadow-2xl p-6 w-full max-w-6xl mx-auto">
        {/* Header */}
        <div className="text-center mb-6">
          <h1 className="text-2xl font-bold text-blue-900 mb-1">
            CardDemo
          </h1>
          <h2 className="text-lg text-gray-600 mb-2">
            User List Management
          </h2>
          <div className="text-sm text-gray-500">
            <p>Program: COUSR00C | Transaction: CU00</p>
            <p>{new Date().toLocaleDateString()} | {new Date().toLocaleTimeString()}</p>
          </div>
        </div>

        {/* Filter Section */}
        <div className="mb-6 bg-gray-50 p-4 rounded-lg">
          <div className="flex items-center space-x-4">
            <label htmlFor="filter" className="text-sm font-medium text-gray-700">
              Search from User ID:
            </label>
            <input
              id="filter"
              type="text"
              value={state.filter}
              onChange={handleFilterChange}
              placeholder="Enter User ID"
              maxLength={8}
              className="w-48 px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 uppercase"
              disabled={state.isSubmitting}
            />
            <Button
              variant="primary"
              onClick={handleFilterSubmit}
              disabled={state.isSubmitting}
              size="sm"
            >
              Search (ENTER)
            </Button>
          </div>
        </div>

        {/* User List Table */}
        <div className="mb-6">
          {state.isLoading ? (
            <div className="text-center py-8">
              <div className="inline-block animate-spin rounded-full h-8 w-8 border-b-2 border-blue-900"></div>
              <p className="mt-2 text-gray-600">Loading users...</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="w-full border-collapse border border-gray-300">
                <thead>
                  <tr className="bg-gray-100">
                    <th className="border border-gray-300 px-4 py-2 text-left">Sel</th>
                    <th className="border border-gray-300 px-4 py-2 text-left">User ID</th>
                    <th className="border border-gray-300 px-4 py-2 text-left">First Name</th>
                    <th className="border border-gray-300 px-4 py-2 text-left">Last Name</th>
                    <th className="border border-gray-300 px-4 py-2 text-left">User Type</th>
                  </tr>
                </thead>
                <tbody>
                  {state.users.map((user, index) => (
                    <UserRow
                      key={user.userId}
                      user={user}
                      index={index}
                      onAction={handleUserAction}
                    />
                  ))}
                  {/* Fill empty rows to maintain 10-row display */}
                  {Array.from({ length: Math.max(0, 10 - state.users.length) }).map((_, index) => (
                    <tr key={`empty-${index}`}>
                      <td className="border border-gray-300 px-4 py-2">&nbsp;</td>
                      <td className="border border-gray-300 px-4 py-2">&nbsp;</td>
                      <td className="border border-gray-300 px-4 py-2">&nbsp;</td>
                      <td className="border border-gray-300 px-4 py-2">&nbsp;</td>
                      <td className="border border-gray-300 px-4 py-2">&nbsp;</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>

        {/* Pagination Info */}
        <div className="mb-4 text-center text-sm text-gray-600">
          <p>
            Page {state.currentPage + 1} of {Math.max(1, state.totalPages)} 
            ({state.totalCount} total users)
          </p>
        </div>

        {/* Action Buttons */}
        <div className="flex flex-wrap justify-center space-x-2 mb-6">
          <Button
            variant="secondary"
            onClick={handlePreviousPage}
            disabled={state.currentPage <= 0 || state.isSubmitting}
            size="sm"
          >
            Previous (PF7)
          </Button>
          
          <Button
            variant="secondary"
            onClick={handleNextPage}
            disabled={state.currentPage >= state.totalPages - 1 || state.isSubmitting}
            size="sm"
          >
            Next (PF8)
          </Button>
          
          <Button
            variant="secondary"
            onClick={handleExit}
            disabled={state.isSubmitting}
            size="sm"
          >
            Exit (PF3)
          </Button>
        </div>

        {/* Messages */}
        {(state.error || state.message) && (
          <div className={`mb-4 p-3 rounded-md ${
            state.error 
              ? 'bg-red-50 border border-red-200 text-red-800' 
              : 'bg-blue-50 border border-blue-200 text-blue-800'
          }`}>
            <p className="text-sm">{state.error || state.message}</p>
          </div>
        )}

        {/* Instructions */}
        <div className="text-center text-sm text-gray-500">
          <p>
            Enter U or D in Sel column, then press ENTER. Use PF7/PF8 to navigate pages, F3 to exit.
          </p>
        </div>
      </div>
    </div>
  );
};

// Individual user row component
interface UserRowProps {
  user: UserSecurityDTO;
  index: number;
  onAction: (userId: string, action: string) => void;
}

const UserRow: React.FC<UserRowProps> = ({ user, index, onAction }) => {
  const [selection, setSelection] = useState('');

  const handleSelectionChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const value = event.target.value.toUpperCase().slice(0, 1);
    setSelection(value);
  };

  const handleKeyDown = (event: React.KeyboardEvent) => {
    if (event.key === 'Enter' && selection) {
      onAction(user.userId, selection);
      setSelection('');
    }
  };

  return (
    <tr className="hover:bg-gray-50">
      <td className="border border-gray-300 px-4 py-2">
        <input
          type="text"
          value={selection}
          onChange={handleSelectionChange}
          onKeyDown={handleKeyDown}
          maxLength={1}
          className="w-8 px-1 py-1 text-center border border-gray-300 rounded focus:outline-none focus:ring-1 focus:ring-blue-500 uppercase"
          placeholder=""
        />
      </td>
      <td className="border border-gray-300 px-4 py-2 font-mono">{user.userId}</td>
      <td className="border border-gray-300 px-4 py-2">{user.firstName}</td>
      <td className="border border-gray-300 px-4 py-2">{user.lastName}</td>
      <td className="border border-gray-300 px-4 py-2">{USER_TYPE_LABELS[user.userType]}</td>
    </tr>
  );
};

export default UserList;