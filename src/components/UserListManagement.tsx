'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { userSecurityService } from '@/services/userSecurityService';
import { 
  UserSecurityDTO, 
  UserSecurityListResponse, 
  UserSecuritySearchCriteria,
  InvalidKeyResponseDTO 
} from '@/types/user-security';

interface UserListManagementProps {
  programName?: string;
  transactionId?: string;
  systemId?: string;
  workstationId?: string;
}

export default function UserListManagement({
  programName = 'COUSR00C',
  transactionId = 'USRLST',
  systemId = 'SYSTEM',
  workstationId = 'WS001'
}: UserListManagementProps) {
  const router = useRouter();
  
  // State management
  const [users, setUsers] = useState<UserSecurityDTO[]>([]);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string>('');
  const [searchUserId, setSearchUserId] = useState<string>('');
  const [currentPage, setCurrentPage] = useState<number>(1);
  const [totalPages, setTotalPages] = useState<number>(1);
  const [totalCount, setTotalCount] = useState<number>(0);
  const [selectedUserIndex, setSelectedUserIndex] = useState<number>(-1);
  const [currentDateTime, setCurrentDateTime] = useState<string>('');
  
  // Constants
  const PAGE_SIZE = 10;

  // Update current date and time
  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      const dateStr = now.toLocaleDateString('en-US', {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit'
      });
      const timeStr = now.toLocaleTimeString('en-US', {
        hour12: false,
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit'
      });
      setCurrentDateTime(`${dateStr} ${timeStr}`);
    };

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    
    return () => clearInterval(interval);
  }, []);

  // Load users data
  const loadUsers = useCallback(async (page: number = 1, userId?: string) => {
    setLoading(true);
    setError('');
    
    try {
      const searchCriteria: UserSecuritySearchCriteria = {
        page,
        pageSize: PAGE_SIZE,
        sortBy: 'userId',
        sortOrder: 'ASC'
      };

      if (userId && userId.trim()) {
        searchCriteria.userId = userId.trim().toUpperCase();
      }

      const response: UserSecurityListResponse = await userSecurityService.listUserSecurity(searchCriteria);
      
      setUsers(response.users);
      setCurrentPage(response.currentPage);
      setTotalPages(response.totalPages);
      setTotalCount(response.totalCount);
      setSelectedUserIndex(-1);
      
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to load users';
      setError(errorMessage);
      setUsers([]);
      setTotalCount(0);
      setTotalPages(1);
    } finally {
      setLoading(false);
    }
  }, []);

  // Initial load
  useEffect(() => {
    loadUsers(1);
  }, [loadUsers]);

  // Handle search
  const handleSearch = useCallback(() => {
    setCurrentPage(1);
    loadUsers(1, searchUserId);
  }, [loadUsers, searchUserId]);

  // Handle previous page (PF7)
  const handlePreviousPage = useCallback(() => {
    if (currentPage > 1) {
      const newPage = currentPage - 1;
      setCurrentPage(newPage);
      loadUsers(newPage, searchUserId);
    }
  }, [currentPage, loadUsers, searchUserId]);

  // Handle next page (PF8)
  const handleNextPage = useCallback(() => {
    if (currentPage < totalPages) {
      const newPage = currentPage + 1;
      setCurrentPage(newPage);
      loadUsers(newPage, searchUserId);
    }
  }, [currentPage, totalPages, loadUsers, searchUserId]);

  // Handle exit (PF3)
  const handleExit = useCallback(() => {
    router.push('/dashboard');
  }, [router]);

  // Handle user selection for update
  const handleUpdateUser = useCallback((userId: string) => {
    router.push(`/user-security/update/${encodeURIComponent(userId)}`);
  }, [router]);

  // Handle user selection for delete
  const handleDeleteUser = useCallback((userId: string) => {
    router.push(`/user-security/delete/${encodeURIComponent(userId)}`);
  }, [router]);

  // Handle invalid key
  const handleInvalidKey = useCallback(async (key: string) => {
    try {
      const currentUserId = userSecurityService.getStoredUserId() || 'GUEST';
      const invalidKeyRequest = {
        userId: currentUserId,
        invalidKey: key,
        reason: `Invalid function key pressed: ${key}`
      };

      const response: InvalidKeyResponseDTO = await userSecurityService.handleInvalidKey(invalidKeyRequest);
      
      if (response.acknowledged) {
        setError(`Invalid key: ${key}. ${response.message}`);
      }
    } catch (err) {
      setError(`Invalid key: ${key}`);
    }
  }, []);

  // Handle keyboard events
  const handleKeyDown = useCallback((event: React.KeyboardEvent) => {
    const key = event.key.toLowerCase();
    
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        if (!loading) {
          handleSearch();
        }
        break;
      case 'F3':
        event.preventDefault();
        if (!loading) {
          handleExit();
        }
        break;
      case 'F7':
        event.preventDefault();
        if (!loading) {
          handlePreviousPage();
        }
        break;
      case 'F8':
        event.preventDefault();
        if (!loading) {
          handleNextPage();
        }
        break;
      case 'F1':
      case 'F2':
      case 'F4':
      case 'F5':
      case 'F6':
      case 'F9':
      case 'F10':
      case 'F11':
      case 'F12':
        event.preventDefault();
        handleInvalidKey(event.key);
        break;
      default:
        if (key === 'u' && selectedUserIndex >= 0) {
          event.preventDefault();
          const selectedUser = users[selectedUserIndex];
          if (selectedUser) {
            handleUpdateUser(selectedUser.userId);
          }
        } else if (key === 'd' && selectedUserIndex >= 0) {
          event.preventDefault();
          const selectedUser = users[selectedUserIndex];
          if (selectedUser) {
            handleDeleteUser(selectedUser.userId);
          }
        }
        break;
    }
  }, [loading, handleSearch, handleExit, handlePreviousPage, handleNextPage, handleInvalidKey, selectedUserIndex, users, handleUpdateUser, handleDeleteUser]);

  // Handle user selection
  const handleUserSelection = useCallback((index: number) => {
    setSelectedUserIndex(index === selectedUserIndex ? -1 : index);
  }, [selectedUserIndex]);

  // Handle search input change
  const handleSearchInputChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const value = event.target.value.toUpperCase();
    setSearchUserId(value);
    setError('');
  };

  // Format user status display
  const formatUserStatus = (user: UserSecurityDTO): string => {
    if (user.accountLocked) return 'LOCKED';
    return user.userStatus;
  };

  // Get status color class
  const getStatusColorClass = (user: UserSecurityDTO): string => {
    if (user.accountLocked || user.userStatus === 'LOCKED') return 'text-red-400';
    if (user.userStatus === 'INACTIVE' || user.userStatus === 'SUSPENDED') return 'text-yellow-400';
    return 'text-green-400';
  };

  return (
    <div 
      className="min-h-screen bg-black text-green-400 font-mono p-4"
      onKeyDown={handleKeyDown}
      tabIndex={0}
    >
      {/* Screen Header */}
      <div className="border border-green-400 mb-4 p-2">
        <div className="flex justify-between items-center">
          <div className="flex space-x-8">
            <span>Program: {programName}</span>
            <span>Transaction: {transactionId}</span>
          </div>
          <div>
            <span>{currentDateTime}</span>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="max-w-7xl mx-auto">
        {/* Title */}
        <div className="text-center mb-6">
          <h1 className="text-2xl mb-2">USER LIST MANAGEMENT</h1>
          <div className="text-sm text-green-300">Manage system user accounts and security settings</div>
        </div>

        {/* Search Section */}
        <div className="border border-green-400 p-4 mb-4">
          <div className="flex items-center space-x-4">
            <label className="text-sm font-bold">Search User ID:</label>
            <div className="flex-1 max-w-md">
              <Input
                type="text"
                value={searchUserId}
                onChange={handleSearchInputChange}
                className="bg-black border-green-400 text-green-400 focus:border-green-300 focus:ring-green-300 font-mono uppercase"
                maxLength={8}
                placeholder="Enter User ID (optional)"
                disabled={loading}
              />
            </div>
            <Button
              onClick={handleSearch}
              disabled={loading}
              className="bg-green-700 hover:bg-green-600 text-white border-green-400 font-mono"
            >
              {loading ? 'Searching...' : 'Search (ENTER)'}
            </Button>
          </div>
        </div>

        {/* Error Message */}
        {error && (
          <div className="border border-red-400 bg-red-900/20 text-red-400 p-3 mb-4">
            <div className="font-bold">ERROR:</div>
            <div>{error}</div>
          </div>
        )}

        {/* Loading Message */}
        {loading && (
          <div className="border border-yellow-400 bg-yellow-900/20 text-yellow-400 p-3 mb-4">
            <div>Loading users... Please wait</div>
          </div>
        )}

        {/* User List Table */}
        <div className="border border-green-400 mb-4">
          {/* Table Header */}
          <div className="bg-green-900/20 p-2 border-b border-green-400">
            <div className="flex justify-between items-center">
              <div className="text-sm font-bold">
                User List - Page {currentPage} of {totalPages} ({totalCount} total users)
              </div>
              <div className="text-xs text-green-300">
                Select user and press 'U' for Update or 'D' for Delete
              </div>
            </div>
          </div>

          {/* Table Content */}
          <div className="p-2">
            {users.length === 0 && !loading ? (
              <div className="text-center py-8 text-green-300">
                No users found. {searchUserId ? 'Try a different search criteria.' : 'No users in the system.'}
              </div>
            ) : (
              <div className="space-y-1">
                {/* Column Headers */}
                <div className="grid grid-cols-12 gap-2 text-xs font-bold border-b border-green-400 pb-1">
                  <div className="col-span-1">Sel</div>
                  <div className="col-span-2">User ID</div>
                  <div className="col-span-3">User Name</div>
                  <div className="col-span-1">Type</div>
                  <div className="col-span-1">Status</div>
                  <div className="col-span-2">Last Signon</div>
                  <div className="col-span-1">Failed</div>
                  <div className="col-span-1">Locked</div>
                </div>

                {/* User Rows */}
                {users.map((user, index) => (
                  <div
                    key={user.userId}
                    className={`grid grid-cols-12 gap-2 text-sm py-1 cursor-pointer hover:bg-green-900/10 ${
                      selectedUserIndex === index ? 'bg-green-900/30 border border-green-300' : ''
                    }`}
                    onClick={() => handleUserSelection(index)}
                  >
                    <div className="col-span-1 text-center">
                      {selectedUserIndex === index ? '>' : ' '}
                    </div>
                    <div className="col-span-2 font-mono">{user.userId}</div>
                    <div className="col-span-3 truncate">{user.userName}</div>
                    <div className="col-span-1">{user.userType}</div>
                    <div className={`col-span-1 ${getStatusColorClass(user)}`}>
                      {formatUserStatus(user)}
                    </div>
                    <div className="col-span-2 text-xs">
                      {user.lastSignonDate ? `${user.lastSignonDate} ${user.lastSignonTime || ''}`.trim() : 'Never'}
                    </div>
                    <div className="col-span-1 text-center">
                      {user.failedSignonAttempts}
                    </div>
                    <div className="col-span-1 text-center">
                      {user.accountLocked ? 'Yes' : 'No'}
                    </div>
                  </div>
                ))}
              </div>
            )}
          </div>
        </div>

        {/* Selected User Actions */}
        {selectedUserIndex >= 0 && users[selectedUserIndex] && (
          <div className="border border-green-400 p-4 mb-4">
            <div className="text-sm font-bold mb-2">Selected User: {users[selectedUserIndex].userId}</div>
            <div className="flex space-x-4">
              <Button
                onClick={() => handleUpdateUser(users[selectedUserIndex].userId)}
                className="bg-blue-700 hover:bg-blue-600 text-white border-blue-400 font-mono"
              >
                Update User (U)
              </Button>
              <Button
                onClick={() => handleDeleteUser(users[selectedUserIndex].userId)}
                variant="destructive"
                className="font-mono"
              >
                Delete User (D)
              </Button>
            </div>
          </div>
        )}

        {/* Pagination Controls */}
        <div className="border border-green-400 p-4 mb-4">
          <div className="flex justify-between items-center">
            <div className="text-sm">
              Showing {users.length} of {totalCount} users
            </div>
            <div className="flex space-x-4">
              <Button
                onClick={handlePreviousPage}
                disabled={loading || currentPage <= 1}
                variant="outline"
                className="border-green-400 text-green-400 hover:bg-green-900/20 font-mono"
              >
                Previous (F7)
              </Button>
              <span className="text-sm self-center">
                Page {currentPage} of {totalPages}
              </span>
              <Button
                onClick={handleNextPage}
                disabled={loading || currentPage >= totalPages}
                variant="outline"
                className="border-green-400 text-green-400 hover:bg-green-900/20 font-mono"
              >
                Next (F8)
              </Button>
            </div>
          </div>
        </div>

        {/* Function Keys */}
        <div className="border border-green-400 p-4 mb-4">
          <div className="text-sm">
            <div className="mb-2 font-bold">Function Keys:</div>
            <div className="grid grid-cols-2 gap-2">
              <div>ENTER = Search/Filter</div>
              <div>F3 = Exit</div>
              <div>F7 = Previous Page</div>
              <div>F8 = Next Page</div>
              <div>U = Update Selected User</div>
              <div>D = Delete Selected User</div>
            </div>
          </div>
        </div>

        {/* Action Buttons */}
        <div className="flex justify-center space-x-4">
          <Button
            onClick={handleSearch}
            disabled={loading}
            className="bg-green-700 hover:bg-green-600 text-white border-green-400 font-mono"
          >
            {loading ? 'Searching...' : 'Search (ENTER)'}
          </Button>
          
          <Button
            onClick={handleExit}
            disabled={loading}
            variant="outline"
            className="border-green-400 text-green-400 hover:bg-green-900/20 font-mono"
          >
            Exit (F3)
          </Button>
        </div>

        {/* System Information */}
        <div className="mt-8 text-center text-xs text-green-300">
          <div>System ID: {systemId} | Workstation: {workstationId}</div>
          <div className="mt-2">© 2025 User Security Management System</div>
        </div>
      </div>
    </div>
  );
}