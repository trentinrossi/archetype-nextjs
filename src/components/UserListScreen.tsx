'use client';

import React, { useState, useEffect } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { UserSecurityDTO, PaginatedResponse } from '@/types/user';
import { userService } from '@/services/userService';

interface UserListScreenProps {
  onUserSelect: (userId: string, action: 'U' | 'D') => void;
  onExit: () => void;
  preSelectedUserId?: string;
}

export function UserListScreen({ onUserSelect, onExit, preSelectedUserId }: UserListScreenProps) {
  const [users, setUsers] = useState<UserSecurityDTO[]>([]);
  const [currentPage, setCurrentPage] = useState(0);
  const [hasNextPage, setHasNextPage] = useState(false);
  const [hasPreviousPage, setHasPreviousPage] = useState(false);
  const [searchFromUserId, setSearchFromUserId] = useState(preSelectedUserId || '');
  const [selectedActions, setSelectedActions] = useState<{ [key: string]: 'U' | 'D' | '' }>({});
  const [message, setMessage] = useState('');
  const [loading, setLoading] = useState(false);

  // COUSR00C business rule: Load users on component mount
  useEffect(() => {
    loadUsers();
  }, []);

  // COUSR00C business rule: Load page of users
  const loadUsers = async (page = 0, searchFrom = '') => {
    setLoading(true);
    setMessage('');

    try {
      const response: PaginatedResponse<UserSecurityDTO> = await userService.getUsers(page, 10, 'userId');
      
      setUsers(response.items);
      setCurrentPage(response.page);
      setHasNextPage(response.hasNext);
      setHasPreviousPage(response.hasPrevious);
      
      if (response.items.length === 0) {
        setMessage('No users found');
      }
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to lookup User...';
      setMessage(errorMessage);
    } finally {
      setLoading(false);
    }
  };

  // COUSR00C business rule: Handle ENTER key (search/filter)
  const handleEnterKey = async () => {
    // Check for user selection first
    const selectedUserId = Object.keys(selectedActions).find(
      userId => selectedActions[userId] && selectedActions[userId] !== ''
    );

    if (selectedUserId) {
      const action = selectedActions[selectedUserId];
      if (action === 'U' || action === 'u') {
        onUserSelect(selectedUserId, 'U');
        return;
      } else if (action === 'D' || action === 'd') {
        onUserSelect(selectedUserId, 'D');
        return;
      } else {
        setMessage('Invalid selection. Valid values are U and D');
        return;
      }
    }

    // If no selection, perform search/filter
    setCurrentPage(0);
    await loadUsers(0, searchFromUserId);
  };

  // COUSR00C business rule: Handle PF7 key (page backward)
  const handlePageBackward = async () => {
    if (!hasPreviousPage) {
      setMessage('You are already at the top of the page...');
      return;
    }

    await loadUsers(currentPage - 1, searchFromUserId);
  };

  // COUSR00C business rule: Handle PF8 key (page forward)
  const handlePageForward = async () => {
    if (!hasNextPage) {
      setMessage('You are already at the bottom of the page...');
      return;
    }

    await loadUsers(currentPage + 1, searchFromUserId);
  };

  // Handle selection change
  const handleSelectionChange = (userId: string, value: string) => {
    const upperValue = value.toUpperCase();
    setSelectedActions(prev => ({
      ...prev,
      [userId]: upperValue as 'U' | 'D' | ''
    }));
    
    // Clear message when user makes a selection
    if (message.includes('Invalid selection')) {
      setMessage('');
    }
  };

  // Handle invalid key press
  const handleInvalidKey = () => {
    setMessage('INVALID KEY PRESSED');
  };

  return (
    <div className="min-h-screen bg-gray-100 p-4">
      <div className="bg-white rounded-lg shadow-md p-6 max-w-6xl mx-auto">
        {/* Screen Header - COUSR00C layout */}
        <div className="mb-6 text-center border-b pb-4">
          <h1 className="text-2xl font-bold text-gray-800 mb-2">CARDDEMO</h1>
          <h2 className="text-lg text-gray-600 mb-4">User List</h2>
          <div className="text-sm text-gray-500 space-y-1">
            <div>Program: COUSR00C | Transaction: CU00</div>
            <div>{new Date().toLocaleDateString('en-US', { 
              month: '2-digit', 
              day: '2-digit', 
              year: '2-digit' 
            })} {new Date().toLocaleTimeString('en-US', { 
              hour12: false 
            })}</div>
          </div>
        </div>

        {/* Search Filter */}
        <div className="mb-4">
          <Input
            label="Search from User ID"
            type="text"
            value={searchFromUserId}
            onChange={(e) => setSearchFromUserId(e.target.value.toUpperCase().substring(0, 8))}
            maxLength={8}
            placeholder="Enter User ID to start from"
          />
        </div>

        {/* User List Table */}
        <div className="mb-4">
          <div className="overflow-x-auto">
            <table className="w-full border-collapse border border-gray-300">
              <thead>
                <tr className="bg-gray-50">
                  <th className="border border-gray-300 px-2 py-2 text-left">Sel</th>
                  <th className="border border-gray-300 px-4 py-2 text-left">User ID</th>
                  <th className="border border-gray-300 px-4 py-2 text-left">User Type</th>
                  <th className="border border-gray-300 px-4 py-2 text-left">Status</th>
                  <th className="border border-gray-300 px-4 py-2 text-left">Type Display</th>
                </tr>
              </thead>
              <tbody>
                {users.map((user, index) => (
                  <tr key={user.userId} className={index % 2 === 0 ? 'bg-white' : 'bg-gray-50'}>
                    <td className="border border-gray-300 px-2 py-2">
                      <input
                        type="text"
                        className="w-8 text-center border rounded px-1"
                        maxLength={1}
                        value={selectedActions[user.userId] || ''}
                        onChange={(e) => handleSelectionChange(user.userId, e.target.value)}
                        placeholder="U/D"
                      />
                    </td>
                    <td className="border border-gray-300 px-4 py-2 font-mono">{user.userId}</td>
                    <td className="border border-gray-300 px-4 py-2">{user.userType}</td>
                    <td className="border border-gray-300 px-4 py-2">
                      <span className={`px-2 py-1 rounded text-xs ${
                        user.active ? 'bg-green-100 text-green-800' : 'bg-red-100 text-red-800'
                      }`}>
                        {user.active ? 'Active' : 'Inactive'}
                      </span>
                    </td>
                    <td className="border border-gray-300 px-4 py-2">{user.userTypeDisplayName}</td>
                  </tr>
                ))}
                
                {/* Fill empty rows to maintain 10-row display like COBOL screen */}
                {Array.from({ length: Math.max(0, 10 - users.length) }).map((_, index) => (
                  <tr key={`empty-${index}`} className={index % 2 === 0 ? 'bg-white' : 'bg-gray-50'}>
                    <td className="border border-gray-300 px-2 py-2">&nbsp;</td>
                    <td className="border border-gray-300 px-4 py-2">&nbsp;</td>
                    <td className="border border-gray-300 px-4 py-2">&nbsp;</td>
                    <td className="border border-gray-300 px-4 py-2">&nbsp;</td>
                    <td className="border border-gray-300 px-4 py-2">&nbsp;</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>

        {/* Page Information */}
        <div className="mb-4 text-center">
          <span className="text-sm text-gray-600">
            Page {currentPage + 1} | Total Users: {users.length}
          </span>
        </div>

        {/* Message Area */}
        {message && (
          <div className={`mb-4 p-3 rounded text-sm ${
            message.includes('Invalid') || message.includes('Unable') || message.includes('already')
              ? 'bg-red-100 text-red-700' 
              : 'bg-blue-100 text-blue-700'
          }`}>
            {message}
          </div>
        )}

        {/* Function Keys - COUSR00C business rules */}
        <div className="space-y-2">
          <Button
            onClick={handleEnterKey}
            className="w-full"
            disabled={loading}
          >
            {loading ? 'Processing...' : 'Process Selection / Search (ENTER)'}
          </Button>

          <div className="grid grid-cols-4 gap-2">
            <Button
              variant="secondary"
              onClick={onExit}
              disabled={loading}
            >
              Exit (PF3)
            </Button>

            <Button
              variant="outline"
              onClick={handlePageBackward}
              disabled={loading || !hasPreviousPage}
            >
              Previous (PF7)
            </Button>

            <Button
              variant="outline"
              onClick={handlePageForward}
              disabled={loading || !hasNextPage}
            >
              Next (PF8)
            </Button>

            <Button
              variant="destructive"
              onClick={handleInvalidKey}
              disabled={loading}
            >
              Invalid Key
            </Button>
          </div>
        </div>

        {/* Instructions */}
        <div className="mt-6 text-xs text-gray-500 text-center space-y-1">
          <p>Enter U (Update) or D (Delete) in the Sel column, then press ENTER</p>
          <p>Use PF7/PF8 to navigate pages | PF3 to exit</p>
          <p>Enter User ID in search field to filter results</p>
        </div>
      </div>
    </div>
  );
}