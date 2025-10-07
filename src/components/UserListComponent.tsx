'use client';

import React, { useState, useEffect, useRef, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { userSecurityService } from '@/services/userSecurityService';
import { UserSecurityDTO, UserSecurityFilters } from '@/types/userSecurity';

interface UserListComponentProps {
  onUserSelect?: (user: UserSecurityDTO) => void;
  onUpdate?: (userId: string) => void;
  onDelete?: (userId: string) => void;
  onExit?: () => void;
  className?: string;
}

const UserListComponent: React.FC<UserListComponentProps> = ({
  onUserSelect,
  onUpdate,
  onDelete,
  onExit,
  className = ''
}) => {
  const router = useRouter();
  const [users, setUsers] = useState<UserSecurityDTO[]>([]);
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [error, setError] = useState<string>('');
  const [selectedUserIndex, setSelectedUserIndex] = useState<number>(-1);
  const [searchUserId, setSearchUserId] = useState<string>('');
  const [currentPage, setCurrentPage] = useState<number>(1);
  const [totalPages, setTotalPages] = useState<number>(1);
  const [totalCount, setTotalCount] = useState<number>(0);
  const [isSearchMode, setIsSearchMode] = useState<boolean>(false);
  const [selections, setSelections] = useState<{ [key: number]: string }>({});

  const searchInputRef = useRef<HTMLInputElement>(null);

  const pageSize = 10;

  const loadUsers = useCallback(async (page: number = 1, userIdFilter?: string) => {
    setIsLoading(true);
    setError('');

    try {
      const filters: UserSecurityFilters = {
        page: page - 1, // API uses 0-based pagination
        pageSize,
        sortBy: 'userId',
        sortOrder: 'asc'
      };

      if (userIdFilter?.trim()) {
        filters.searchTerm = userIdFilter.trim();
      }

      const response = await userSecurityService.getUsers(filters);

      if (response.success && response.data) {
        const data = response.data;
        setUsers(data.users);
        setTotalPages(data.totalPages);
        setTotalCount(data.totalCount);
        setCurrentPage(data.page + 1); // Convert back to 1-based
        setSelectedUserIndex(-1);
        setSelections({});
      } else {
        setError(response.error?.message || 'Unable to lookup User...');
        setUsers([]);
        setTotalPages(1);
        setTotalCount(0);
        setSelectedUserIndex(-1);
      }
    } catch (err) {
      console.error('Error loading users:', err);
      setError('Unable to lookup User...');
      setUsers([]);
      setTotalPages(1);
      setTotalCount(0);
      setSelectedUserIndex(-1);
    } finally {
      setIsLoading(false);
    }
  }, []);

  useEffect(() => {
    loadUsers(1);
    // Focus on search input when component mounts
    if (searchInputRef.current) {
      searchInputRef.current.focus();
    }
  }, [loadUsers]);

  const handleSearch = async () => {
    if (searchUserId.trim()) {
      setIsSearchMode(true);
      await loadUsers(1, searchUserId);
    } else {
      setIsSearchMode(false);
      await loadUsers(1);
    }
  };

  const handleClear = async () => {
    setSearchUserId('');
    setIsSearchMode(false);
    setSelections({});
    await loadUsers(1);
    if (searchInputRef.current) {
      searchInputRef.current.focus();
    }
  };

  const handleExit = () => {
    if (onExit) {
      onExit();
    } else {
      router.push('/admin/dashboard');
    }
  };

  const handlePreviousPage = async () => {
    if (currentPage > 1) {
      await loadUsers(currentPage - 1, isSearchMode ? searchUserId : undefined);
    } else {
      setError('You are already at the top of the page...');
    }
  };

  const handleNextPage = async () => {
    if (currentPage < totalPages) {
      await loadUsers(currentPage + 1, isSearchMode ? searchUserId : undefined);
    } else {
      setError('You are already at the bottom of the page...');
    }
  };

  const handleSelectionChange = (index: number, value: string) => {
    const newSelections = { ...selections };
    if (value.trim() === '') {
      delete newSelections[index];
    } else {
      newSelections[index] = value.toUpperCase();
    }
    setSelections(newSelections);
  };

  const processSelection = () => {
    // Find the first selection
    const selectedIndex = Object.keys(selections).find(key => selections[parseInt(key)]);
    
    if (selectedIndex) {
      const index = parseInt(selectedIndex);
      const action = selections[index];
      const user = users[index];
      
      if (!user) return;
      
      switch (action) {
        case 'U':
          if (onUpdate) {
            onUpdate(user.userId);
          } else {
            router.push(`/users/${user.userId}/edit`);
          }
          break;
        case 'D':
          if (onDelete) {
            onDelete(user.userId);
          } else {
            router.push(`/users/${user.userId}/delete`);
          }
          break;
        default:
          setError('Invalid selection. Valid values are U and D');
          break;
      }
    } else {
      // No selection made, just perform search
      handleSearch();
    }
  };

  const handleKeyDown = async (event: React.KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        processSelection();
        break;

      case 'F3':
        event.preventDefault();
        handleExit();
        break;

      case 'F4':
        event.preventDefault();
        await handleClear();
        break;

      case 'F7':
        event.preventDefault();
        await handlePreviousPage();
        break;

      case 'F8':
        event.preventDefault();
        await handleNextPage();
        break;

      default:
        // Check for other function keys and show invalid key message
        if (event.key.startsWith('F')) {
          event.preventDefault();
          setError('INVALID KEY PRESSED');
        }
        break;
    }
  };

  return (
    <div 
      className={`min-h-screen bg-gray-50 p-6 ${className}`}
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      <div className="max-w-7xl mx-auto">
        {/* Header */}
        <div className="mb-6">
          <h1 className="text-2xl font-bold text-gray-900 mb-2">
            CardDemo
          </h1>
          <h2 className="text-xl text-gray-700 mb-2">
            User List
          </h2>
          <div className="flex justify-between text-sm text-gray-600">
            <span>COUSR00C - CU00</span>
            <span>{new Date().toLocaleDateString()} {new Date().toLocaleTimeString()}</span>
          </div>
        </div>

        {/* Search Section */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-4 mb-6">
          <div className="flex items-center space-x-4">
            <div className="flex-1">
              <label htmlFor="searchUserId" className="block text-sm font-medium text-gray-700 mb-1">
                Search from User ID:
              </label>
              <input
                ref={searchInputRef}
                id="searchUserId"
                type="text"
                value={searchUserId}
                onChange={(e) => setSearchUserId(e.target.value)}
                placeholder="Enter User ID..."
                className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
                disabled={isLoading}
                maxLength={8}
              />
            </div>
          </div>
        </div>

        {/* Error Message */}
        {error && (
          <div className="bg-red-50 border border-red-200 rounded-md p-4 mb-4">
            <p className="text-sm text-red-800">{error}</p>
          </div>
        )}

        {/* Users Table */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 mb-6">
          <div className="overflow-x-auto">
            <table className="min-w-full divide-y divide-gray-200">
              <thead className="bg-gray-50">
                <tr>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Sel
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    User ID
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    First Name
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Last Name
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    User Type
                  </th>
                </tr>
              </thead>
              <tbody className="bg-white divide-y divide-gray-200">
                {isLoading ? (
                  <tr>
                    <td colSpan={5} className="px-6 py-4 text-center text-gray-500">
                      Loading users...
                    </td>
                  </tr>
                ) : users.length === 0 ? (
                  <tr>
                    <td colSpan={5} className="px-6 py-4 text-center text-gray-500">
                      {isSearchMode ? `No users found matching "${searchUserId}"` : 'No users found'}
                    </td>
                  </tr>
                ) : (
                  users.map((user, index) => (
                    <tr key={user.userId} className={index % 2 === 0 ? 'bg-white' : 'bg-gray-50'}>
                      <td className="px-6 py-4 whitespace-nowrap text-sm">
                        <input
                          type="text"
                          value={selections[index] || ''}
                          onChange={(e) => handleSelectionChange(index, e.target.value)}
                          className="w-8 px-1 py-1 border border-gray-300 rounded text-center focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
                          maxLength={1}
                          disabled={isLoading}
                        />
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                        {user.userId}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {user.firstName}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {user.lastName}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {user.userType}
                      </td>
                    </tr>
                  ))
                )}
                {/* Fill empty rows to maintain 10-row display */}
                {users.length < 10 && !isLoading && Array.from({ length: 10 - users.length }).map((_, index) => (
                  <tr key={`empty-${index}`} className={(users.length + index) % 2 === 0 ? 'bg-white' : 'bg-gray-50'}>
                    <td className="px-6 py-4 whitespace-nowrap text-sm">&nbsp;</td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm">&nbsp;</td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm">&nbsp;</td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm">&nbsp;</td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm">&nbsp;</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>

        {/* Page Information */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-4 mb-6">
          <div className="flex justify-between items-center text-sm text-gray-600">
            <div>
              Page {currentPage} of {totalPages} ({totalCount} total users)
            </div>
            <div>
              Press PF5 key to save your updates...
            </div>
          </div>
        </div>

        {/* Function Key Instructions */}
        <div className="bg-gray-50 border border-gray-200 rounded-md p-4">
          <h3 className="text-sm font-medium text-gray-900 mb-2">Instructions</h3>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-2 text-xs text-gray-600">
            <div><span className="font-medium">Enter:</span> Search or process selection</div>
            <div><span className="font-medium">F3:</span> Exit to Admin Menu</div>
            <div><span className="font-medium">F4:</span> Clear search</div>
            <div><span className="font-medium">F7:</span> Previous page</div>
            <div><span className="font-medium">F8:</span> Next page</div>
            <div><span className="font-medium">U:</span> Update user (in Sel column)</div>
            <div><span className="font-medium">D:</span> Delete user (in Sel column)</div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default UserListComponent;