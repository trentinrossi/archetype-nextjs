'use client';

import React, { useState, useEffect } from 'react';
import { Button } from '@/components/ui/Button';
import { UserSecurityDTO } from '@/types/user';
import { userService } from '@/services/userService';

interface UserListComponentProps {
  onUserSelect: (userId: string, action: 'U' | 'D') => void;
  onExit: () => void;
}

export function UserListComponent({ onUserSelect, onExit }: UserListComponentProps) {
  const [users, setUsers] = useState<UserSecurityDTO[]>([]);
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [hasNextPage, setHasNextPage] = useState(false);
  const [hasPreviousPage, setHasPreviousPage] = useState(false);
  const [searchUserId, setSearchUserId] = useState('');
  const [message, setMessage] = useState('');
  const [messageType, setMessageType] = useState<'success' | 'error' | 'info'>('info');
  const [loading, setLoading] = useState(false);
  const [selections, setSelections] = useState<{ [key: string]: 'U' | 'D' | '' }>({});

  // Load users on component mount
  useEffect(() => {
    loadUsers();
  }, []);

  // Load users with pagination (COUSR00C business logic)
  const loadUsers = async (page: number = 0, startFromUserId?: string) => {
    setLoading(true);
    setMessage('');

    try {
      const response = await userService.getAllUsers({
        page,
        size: 10, // Display 10 users per page (COBOL business logic)
        sort: 'userId'
      });

      setUsers(response.users);
      setCurrentPage(response.pagination.page);
      setTotalPages(response.pagination.totalPages);
      setHasNextPage(response.pagination.page < response.pagination.totalPages - 1);
      setHasPreviousPage(response.pagination.page > 0);
      
      // Clear selections when loading new page
      setSelections({});

      if (response.users.length === 0) {
        setMessage('No users found');
        setMessageType('info');
      }

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to lookup User...';
      setMessage(errorMessage);
      setMessageType('error');
    } finally {
      setLoading(false);
    }
  };

  // Handle ENTER key - search from specific User ID
  const handleEnterKey = () => {
    if (searchUserId.trim() === '') {
      // Start from beginning if no search ID provided
      loadUsers(0);
    } else {
      // In a real implementation, this would search from the specific User ID
      // For now, we'll just search from the beginning
      setMessage(`Searching from User ID: ${searchUserId.toUpperCase()}`);
      setMessageType('info');
      loadUsers(0);
    }
  };

  // Handle PF7 key - page backward
  const handlePF7Key = () => {
    if (hasPreviousPage) {
      loadUsers(currentPage - 1);
    } else {
      setMessage('You are already at the top of the page...');
      setMessageType('info');
    }
  };

  // Handle PF8 key - page forward
  const handlePF8Key = () => {
    if (hasNextPage) {
      loadUsers(currentPage + 1);
    } else {
      setMessage('You are already at the bottom of the page...');
      setMessageType('info');
    }
  };

  // Handle PF3 key - exit
  const handlePF3Key = () => {
    onExit();
  };

  // Handle user selection
  const handleSelectionChange = (userId: string, action: 'U' | 'D' | '') => {
    setSelections(prev => ({
      ...prev,
      [userId]: action
    }));
  };

  // Process user selection (COUSR00C business logic)
  const processSelection = () => {
    // Find the first non-empty selection
    for (const [userId, action] of Object.entries(selections)) {
      if (action === 'U' || action === 'D') {
        // Validate selection
        if (action !== 'U' && action !== 'D') {
          setMessage('Invalid selection. Valid values are U and D');
          setMessageType('error');
          return;
        }
        
        onUserSelect(userId, action);
        return;
      }
    }

    setMessage('Please select a user and action (U for Update, D for Delete)');
    setMessageType('error');
  };

  // Handle form submission
  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    
    // Check if there's a selection to process
    const hasSelection = Object.values(selections).some(action => action === 'U' || action === 'D');
    
    if (hasSelection) {
      processSelection();
    } else {
      handleEnterKey();
    }
  };

  // Handle key press events (simulating COBOL function keys)
  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === 'F3' || (e.altKey && e.key === '3')) {
      e.preventDefault();
      handlePF3Key();
    } else if (e.key === 'F7' || (e.altKey && e.key === '7')) {
      e.preventDefault();
      handlePF7Key();
    } else if (e.key === 'F8' || (e.altKey && e.key === '8')) {
      e.preventDefault();
      handlePF8Key();
    }
  };

  return (
    <div className="min-h-screen bg-gray-900 text-green-400 font-mono">
      {/* Header */}
      <div className="bg-gray-800 p-4 border-b border-green-400">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-xl font-bold">CARDDEMO</h1>
            <h2 className="text-lg">User List</h2>
          </div>
          <div className="text-right">
            <div>TRAN: CU00</div>
            <div>PGM: COUSR00C</div>
            <div>{new Date().toLocaleDateString('en-US', { 
              month: '2-digit', 
              day: '2-digit', 
              year: '2-digit' 
            })}</div>
            <div>{new Date().toLocaleTimeString('en-US', { 
              hour12: false,
              hour: '2-digit',
              minute: '2-digit',
              second: '2-digit'
            })}</div>
          </div>
        </div>
      </div>

      {/* Main content */}
      <div className="p-6">
        <form onSubmit={handleSubmit} onKeyDown={handleKeyPress}>
          {/* Search field */}
          <div className="mb-6">
            <div className="flex items-center space-x-4">
              <label htmlFor="searchUserId" className="w-40">
                Search from User ID:
              </label>
              <input
                id="searchUserId"
                type="text"
                value={searchUserId}
                onChange={(e) => setSearchUserId(e.target.value.toUpperCase().slice(0, 8))}
                className="bg-gray-800 border border-green-400 text-green-400 px-2 py-1 w-32 focus:outline-none focus:border-yellow-400"
                maxLength={8}
                disabled={loading}
              />
              <span className="text-gray-500">(Leave blank to start from beginning)</span>
            </div>
          </div>

          {/* User list table */}
          <div className="mb-6">
            <div className="border border-green-400 bg-gray-800">
              {/* Table header */}
              <div className="grid grid-cols-6 gap-4 p-2 border-b border-green-400 bg-gray-700">
                <div className="font-bold">Sel</div>
                <div className="font-bold">User ID</div>
                <div className="font-bold">First Name</div>
                <div className="font-bold">Last Name</div>
                <div className="font-bold">User Type</div>
                <div className="font-bold">Status</div>
              </div>

              {/* Table rows */}
              {loading ? (
                <div className="p-4 text-center">Loading users...</div>
              ) : users.length === 0 ? (
                <div className="p-4 text-center">No users found</div>
              ) : (
                users.map((user, index) => (
                  <div key={user.userId} className="grid grid-cols-6 gap-4 p-2 border-b border-gray-600 hover:bg-gray-700">
                    <div>
                      <select
                        value={selections[user.userId] || ''}
                        onChange={(e) => handleSelectionChange(user.userId, e.target.value as 'U' | 'D' | '')}
                        className="bg-gray-800 border border-green-400 text-green-400 w-12 focus:outline-none focus:border-yellow-400"
                        disabled={loading}
                      >
                        <option value="">_</option>
                        <option value="U">U</option>
                        <option value="D">D</option>
                      </select>
                    </div>
                    <div>{user.userId}</div>
                    <div>{user.userType === 'ADMIN' ? 'System' : 'General'}</div>
                    <div>{user.userType === 'ADMIN' ? 'Administrator' : 'User'}</div>
                    <div>{user.userType}</div>
                    <div className={user.active ? 'text-green-400' : 'text-red-400'}>
                      {user.active ? 'Active' : 'Inactive'}
                    </div>
                  </div>
                ))
              )}
            </div>

            {/* Page information */}
            <div className="mt-2 text-center">
              Page {currentPage + 1} of {totalPages || 1}
            </div>
          </div>

          {/* Action buttons */}
          <div className="flex space-x-4 mb-6">
            <Button
              type="submit"
              disabled={loading}
              className="bg-green-700 hover:bg-green-600 text-white"
            >
              ENTER - Search/Select
            </Button>
            
            <Button
              type="button"
              onClick={handlePF7Key}
              disabled={loading || !hasPreviousPage}
              className="bg-gray-700 hover:bg-gray-600 text-white disabled:opacity-50"
            >
              PF7 - Page Up
            </Button>
            
            <Button
              type="button"
              onClick={handlePF8Key}
              disabled={loading || !hasNextPage}
              className="bg-gray-700 hover:bg-gray-600 text-white disabled:opacity-50"
            >
              PF8 - Page Down
            </Button>
            
            <Button
              type="button"
              onClick={handlePF3Key}
              variant="secondary"
              disabled={loading}
              className="bg-gray-700 hover:bg-gray-600 text-white"
            >
              PF3 - Exit
            </Button>
          </div>

          {/* Message area */}
          {message && (
            <div className={`p-3 border ${
              messageType === 'success' ? 'border-green-400 text-green-400' :
              messageType === 'error' ? 'border-red-400 text-red-400' :
              'border-yellow-400 text-yellow-400'
            } bg-gray-800 mb-6`}>
              {message}
            </div>
          )}

          {/* Instructions */}
          <div className="text-gray-500 text-sm">
            <p>Instructions:</p>
            <ul className="list-disc list-inside mt-2 space-y-1">
              <li>Select U (Update) or D (Delete) for a user, then press ENTER</li>
              <li>Use PF7/PF8 to navigate between pages</li>
              <li>Enter a User ID to search from that position</li>
              <li>Press PF3 to return to the Admin Menu</li>
            </ul>
          </div>
        </form>
      </div>
    </div>
  );
}