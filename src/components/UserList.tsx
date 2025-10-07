'use client';

import React, { useState, useEffect, useRef, KeyboardEvent, useCallback } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { Modal } from '@/components/ui/Modal';
import { Table } from '@/components/ui/Table';
import { userSecurityService } from '@/services/userSecurityService';
import { 
  UserSecurityDTO, 
  UserSecurityQuery, 
  PaginatedResponse,
  ValidationError 
} from '@/types/userSecurity';

interface UserListProps {
  onUserUpdate?: (userId: string) => void;
  onUserDelete?: (userId: string) => void;
  onExit?: () => void;
  className?: string;
}

interface UserListState {
  users: UserSecurityDTO[];
  currentPage: number;
  totalPages: number;
  totalCount: number;
  pageSize: number;
  searchUserId: string;
  isLoading: boolean;
  error: string | null;
  selectedActions: Record<string, 'U' | 'D' | ''>;
}

interface ModalState {
  isOpen: boolean;
  title: string;
  message: string;
  type: 'success' | 'error' | 'info' | 'confirm';
  onConfirm?: () => void;
}

const UserList: React.FC<UserListProps> = ({
  onUserUpdate,
  onUserDelete,
  onExit,
  className = ''
}) => {
  const [state, setState] = useState<UserListState>({
    users: [],
    currentPage: 1,
    totalPages: 0,
    totalCount: 0,
    pageSize: 10,
    searchUserId: '',
    isLoading: false,
    error: null,
    selectedActions: {}
  });

  const [modal, setModal] = useState<ModalState>({
    isOpen: false,
    title: '',
    message: '',
    type: 'info'
  });

  const searchInputRef = useRef<HTMLInputElement>(null);
  const tableRef = useRef<HTMLDivElement>(null);

  // Load users on component mount and when page changes
  useEffect(() => {
    loadUsers();
  }, [state.currentPage]);

  // Focus on search input when component mounts
  useEffect(() => {
    if (searchInputRef.current) {
      searchInputRef.current.focus();
    }
  }, []);

  const loadUsers = useCallback(async (searchFromUserId?: string) => {
    setState(prev => ({ ...prev, isLoading: true, error: null }));

    try {
      const query: UserSecurityQuery = {
        page: state.currentPage,
        pageSize: state.pageSize,
        sortBy: 'id',
        sortOrder: 'asc'
      };

      // If searching from a specific User ID, add search parameter
      if (searchFromUserId && searchFromUserId.trim()) {
        query.search = searchFromUserId.trim().toUpperCase();
      }

      const response: PaginatedResponse<UserSecurityDTO> = await userSecurityService.getAllUsers(query);

      setState(prev => ({
        ...prev,
        users: response.data || [],
        totalPages: response.pagination?.totalPages || 0,
        totalCount: response.totalCount || 0,
        isLoading: false,
        selectedActions: {}
      }));

      // Show message if no users found
      if (!response.data || response.data.length === 0) {
        if (searchFromUserId) {
          showModal('NO USERS FOUND', `No users found starting from User ID: ${searchFromUserId}`, 'info');
        } else {
          showModal('NO USERS FOUND', 'No users exist in the system', 'info');
        }
      }

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Failed to load users';
      setState(prev => ({ 
        ...prev, 
        isLoading: false, 
        error: errorMessage,
        users: []
      }));
      showModal('ERROR', `Error loading users: ${errorMessage}`, 'error');
    }
  }, [state.currentPage, state.pageSize]);

  const handleSearch = useCallback(() => {
    if (state.searchUserId.trim()) {
      // Reset to first page and search from specific User ID
      setState(prev => ({ ...prev, currentPage: 1 }));
      loadUsers(state.searchUserId.trim());
    } else {
      // Reset to first page and load all users
      setState(prev => ({ ...prev, currentPage: 1 }));
      loadUsers();
    }
  }, [state.searchUserId, loadUsers]);

  const handlePreviousPage = useCallback(() => {
    if (state.currentPage > 1) {
      setState(prev => ({ ...prev, currentPage: prev.currentPage - 1 }));
    } else {
      showModal('NAVIGATION', 'Already at first page', 'info');
    }
  }, [state.currentPage]);

  const handleNextPage = useCallback(() => {
    if (state.currentPage < state.totalPages) {
      setState(prev => ({ ...prev, currentPage: prev.currentPage + 1 }));
    } else {
      showModal('NAVIGATION', 'Already at last page', 'info');
    }
  }, [state.currentPage, state.totalPages]);

  const handleExit = useCallback(() => {
    if (onExit) {
      onExit();
    } else {
      window.location.href = '/';
    }
  }, [onExit]);

  const handleActionSelection = useCallback((userId: string, action: 'U' | 'D' | '') => {
    setState(prev => ({
      ...prev,
      selectedActions: {
        ...prev.selectedActions,
        [userId]: action
      }
    }));
  }, []);

  const handleUserUpdate = useCallback((userId: string) => {
    if (onUserUpdate) {
      onUserUpdate(userId);
    } else {
      showModal('UPDATE', `Navigate to update screen for User ID: ${userId}`, 'info');
    }
  }, [onUserUpdate]);

  const handleUserDelete = useCallback((userId: string) => {
    const user = state.users.find(u => u.id === userId);
    if (user) {
      showModal(
        'CONFIRM DELETE',
        `Are you sure you want to delete user ${userId} (${user.firstName} ${user.lastName})?`,
        'confirm',
        () => confirmUserDelete(userId)
      );
    }
  }, [state.users]);

  const confirmUserDelete = useCallback(async (userId: string) => {
    try {
      await userSecurityService.deleteUser(userId);
      showModal('SUCCESS', `User ${userId} has been deleted successfully`, 'success');
      
      // Reload current page
      setTimeout(() => {
        loadUsers();
      }, 1500);

      if (onUserDelete) {
        onUserDelete(userId);
      }
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Failed to delete user';
      showModal('ERROR', `Error deleting user: ${errorMessage}`, 'error');
    }
  }, [loadUsers, onUserDelete]);

  const processSelectedActions = useCallback(() => {
    const actionsToProcess = Object.entries(state.selectedActions).filter(([_, action]) => action !== '');
    
    if (actionsToProcess.length === 0) {
      showModal('NO ACTIONS', 'No actions selected. Use U for update or D for delete.', 'info');
      return;
    }

    // Process each selected action
    actionsToProcess.forEach(([userId, action]) => {
      if (action === 'U') {
        handleUserUpdate(userId);
      } else if (action === 'D') {
        handleUserDelete(userId);
      }
    });
  }, [state.selectedActions, handleUserUpdate, handleUserDelete]);

  const handleKeyPress = useCallback((event: KeyboardEvent<HTMLDivElement>) => {
    const key = event.key;

    // Handle ENTER key for search
    if (key === 'Enter') {
      event.preventDefault();
      if (document.activeElement === searchInputRef.current) {
        handleSearch();
      } else {
        processSelectedActions();
      }
      return;
    }

    // Handle F7 key for previous page
    if (key === 'F7') {
      event.preventDefault();
      handlePreviousPage();
      return;
    }

    // Handle F8 key for next page
    if (key === 'F8') {
      event.preventDefault();
      handleNextPage();
      return;
    }

    // Handle F3 key for exit
    if (key === 'F3' || key === 'Escape') {
      event.preventDefault();
      handleExit();
      return;
    }
  }, [handleSearch, processSelectedActions, handlePreviousPage, handleNextPage, handleExit]);

  const showModal = useCallback((title: string, message: string, type: ModalState['type'], onConfirm?: () => void) => {
    setModal({
      isOpen: true,
      title,
      message,
      type,
      onConfirm
    });
  }, []);

  const closeModal = useCallback(() => {
    setModal(prev => ({ ...prev, isOpen: false, onConfirm: undefined }));
  }, []);

  const handleModalConfirm = useCallback(() => {
    if (modal.onConfirm) {
      modal.onConfirm();
    }
    closeModal();
  }, [modal.onConfirm, closeModal]);

  const handleSearchInputChange = useCallback((value: string) => {
    // Convert to uppercase and limit to 8 characters
    const formattedValue = value.toUpperCase().slice(0, 8);
    setState(prev => ({ ...prev, searchUserId: formattedValue }));
  }, []);

  const handleSearchKeyPress = useCallback((event: KeyboardEvent<HTMLInputElement>) => {
    const key = event.key;

    // Only allow alphanumeric characters for User ID search
    if (!/^[A-Za-z0-9]$/.test(key) && !['Backspace', 'Delete', 'ArrowLeft', 'ArrowRight', 'Home', 'End', 'Enter', 'Tab'].includes(key)) {
      event.preventDefault();
      showModal('INVALID KEY', 'User ID accepts only letters and numbers', 'error');
    }
  }, [showModal]);

  // Table columns configuration
  const columns = [
    { key: 'selection' as const, label: 'SEL', width: '60px', align: 'center' as const },
    { key: 'id' as const, label: 'USER ID', width: '100px', align: 'left' as const },
    { key: 'firstName' as const, label: 'FIRST NAME', width: '150px', align: 'left' as const },
    { key: 'lastName' as const, label: 'LAST NAME', width: '150px', align: 'left' as const },
    { key: 'userType' as const, label: 'TYPE', width: '80px', align: 'center' as const }
  ];

  // Render table rows
  const renderTableRows = () => {
    if (state.isLoading) {
      return (
        <tr>
          <td colSpan={5} className="text-center py-8 text-gray-500">
            Loading users...
          </td>
        </tr>
      );
    }

    if (state.users.length === 0) {
      return (
        <tr>
          <td colSpan={5} className="text-center py-8 text-gray-500">
            No users found
          </td>
        </tr>
      );
    }

    return state.users.map((user) => (
      <tr key={user.id} className="border-b border-gray-200 hover:bg-gray-50">
        <td className="px-3 py-2 text-center">
          <select
            value={state.selectedActions[user.id] || ''}
            onChange={(e) => handleActionSelection(user.id, e.target.value as 'U' | 'D' | '')}
            className="w-12 text-xs border border-gray-300 rounded px-1 py-1 focus:outline-none focus:ring-2 focus:ring-blue-500"
          >
            <option value="">-</option>
            <option value="U">U</option>
            <option value="D">D</option>
          </select>
        </td>
        <td className="px-3 py-2 font-mono text-sm">{user.id}</td>
        <td className="px-3 py-2 text-sm">{user.firstName}</td>
        <td className="px-3 py-2 text-sm">{user.lastName}</td>
        <td className="px-3 py-2 text-center text-sm font-mono">{user.userType}</td>
      </tr>
    ));
  };

  return (
    <>
      <div 
        className={`max-w-6xl mx-auto mt-8 p-6 bg-white border border-gray-300 shadow-lg ${className}`}
        onKeyDown={handleKeyPress}
        tabIndex={0}
      >
        {/* Header */}
        <div className="text-center mb-6">
          <h1 className="text-2xl font-bold text-gray-800 mb-2">USER LIST - COUSR00C</h1>
          <div className="text-sm text-gray-600 grid grid-cols-2 gap-4">
            <div>
              <p>ENTER - Search/Process Actions</p>
              <p>F7 - Previous Page</p>
            </div>
            <div>
              <p>F8 - Next Page</p>
              <p>F3/ESC - Exit</p>
            </div>
          </div>
        </div>

        {/* Search Section */}
        <div className="mb-6 bg-gray-50 p-4 border border-gray-200 rounded">
          <div className="flex items-center gap-4">
            <label htmlFor="searchUserId" className="text-sm font-medium text-gray-700 whitespace-nowrap">
              SEARCH FROM USER ID:
            </label>
            <Input
              ref={searchInputRef}
              id="searchUserId"
              name="searchUserId"
              type="text"
              value={state.searchUserId}
              onChange={(e) => handleSearchInputChange(e.target.value)}
              onKeyPress={handleSearchKeyPress}
              className="font-mono uppercase w-32"
              placeholder="USER ID"
              maxLength={8}
              disabled={state.isLoading}
            />
            <Button
              onClick={handleSearch}
              variant="primary"
              size="sm"
              disabled={state.isLoading}
            >
              SEARCH
            </Button>
            <Button
              onClick={() => {
                setState(prev => ({ ...prev, searchUserId: '', currentPage: 1 }));
                loadUsers();
              }}
              variant="outline"
              size="sm"
              disabled={state.isLoading}
            >
              CLEAR
            </Button>
          </div>
        </div>

        {/* Pagination Info */}
        <div className="mb-4 flex justify-between items-center text-sm text-gray-600">
          <div>
            Page {state.currentPage} of {state.totalPages} 
            {state.totalCount > 0 && ` (${state.totalCount} total users)`}
          </div>
          <div className="text-xs">
            Selection: U=Update, D=Delete
          </div>
        </div>

        {/* Users Table */}
        <div ref={tableRef} className="border border-gray-300 rounded-lg overflow-hidden mb-6">
          <table className="w-full">
            <thead className="bg-gray-100">
              <tr>
                {columns.map((column) => (
                  <th
                    key={column.key}
                    className={`px-3 py-3 text-xs font-bold text-gray-700 uppercase tracking-wider border-b border-gray-300 ${
                      column.align === 'center' ? 'text-center' : 
                      column.align === 'right' ? 'text-right' : 'text-left'
                    }`}
                    style={{ width: column.width }}
                  >
                    {column.label}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody className="bg-white">
              {renderTableRows()}
            </tbody>
          </table>
        </div>

        {/* Action Buttons */}
        <div className="flex justify-between items-center">
          <div className="flex gap-3">
            <Button
              onClick={handlePreviousPage}
              variant="outline"
              size="md"
              disabled={state.isLoading || state.currentPage <= 1}
            >
              F7 - PREVIOUS
            </Button>
            <Button
              onClick={handleNextPage}
              variant="outline"
              size="md"
              disabled={state.isLoading || state.currentPage >= state.totalPages}
            >
              F8 - NEXT
            </Button>
          </div>

          <div className="flex gap-3">
            <Button
              onClick={processSelectedActions}
              variant="primary"
              size="md"
              disabled={state.isLoading}
            >
              PROCESS ACTIONS
            </Button>
            <Button
              onClick={handleExit}
              variant="outline"
              size="md"
              disabled={state.isLoading}
            >
              F3 - EXIT
            </Button>
          </div>
        </div>

        {/* Status Messages */}
        {state.error && (
          <div className="mt-4 bg-red-50 border border-red-200 rounded-md p-3">
            <p className="text-red-700 text-sm font-medium">{state.error}</p>
          </div>
        )}

        {/* Instructions */}
        <div className="mt-6 text-center text-xs text-gray-500 border-t border-gray-200 pt-4">
          <p>Use selection dropdown to choose U (Update) or D (Delete) for each user</p>
          <p>Press ENTER to process selected actions, F7/F8 for navigation, F3 to exit</p>
          <p>Search field accepts User ID (8 characters max, letters and numbers only)</p>
        </div>
      </div>

      {/* Modal Dialog */}
      <Modal
        isOpen={modal.isOpen}
        onClose={closeModal}
        title={modal.title}
        className="max-w-md"
      >
        <div className="text-center py-4">
          <div className={`text-base font-medium mb-4 ${
            modal.type === 'success' ? 'text-green-700' : 
            modal.type === 'error' ? 'text-red-700' : 
            modal.type === 'confirm' ? 'text-orange-700' :
            'text-blue-700'
          }`}>
            {modal.message}
          </div>
          <div className="flex gap-3 justify-center">
            {modal.type === 'confirm' ? (
              <>
                <Button
                  onClick={handleModalConfirm}
                  variant="primary"
                  size="sm"
                >
                  YES
                </Button>
                <Button
                  onClick={closeModal}
                  variant="outline"
                  size="sm"
                >
                  NO
                </Button>
              </>
            ) : (
              <Button
                onClick={closeModal}
                variant={modal.type === 'success' ? 'primary' : 'outline'}
                size="sm"
              >
                OK
              </Button>
            )}
          </div>
        </div>
      </Modal>
    </>
  );
};

export default UserList;