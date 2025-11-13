'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { userService } from '@/services/userService';
import { User } from '@/types/user';
import { Button, Input } from '@/components/ui';

export default function DeleteUserPage() {
  const params = useParams();
  const router = useRouter();
  const [user, setUser] = useState<User | null>(null);
  const [userId, setUserId] = useState('');
  const [loading, setLoading] = useState(false);
  const [errorMessage, setErrorMessage] = useState('');
  const [successMessage, setSuccessMessage] = useState('');
  const [currentDate, setCurrentDate] = useState('');
  const [currentTime, setCurrentTime] = useState('');

  useEffect(() => {
    const currentUser = userService.getCurrentUser();
    if (!currentUser || currentUser.userType !== 'A') {
      router.push('/login');
      return;
    }

    if (params.id && params.id !== 'new') {
      fetchUser(params.id as string);
    }

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, [params.id, router]);

  const updateDateTime = () => {
    const now = new Date();
    setCurrentDate(now.toLocaleDateString('en-US', { 
      month: '2-digit', 
      day: '2-digit', 
      year: 'numeric' 
    }));
    setCurrentTime(now.toLocaleTimeString('en-US', { 
      hour: '2-digit', 
      minute: '2-digit', 
      second: '2-digit',
      hour12: false 
    }));
  };

  const fetchUser = async (id: string) => {
    try {
      const data = await userService.getUserById(parseInt(id));
      setUser(data);
      setUserId(data.userId);
      setSuccessMessage('Press Delete button to delete this user ...');
    } catch (err) {
      console.error('Failed to load user:', err);
      setErrorMessage('User ID NOT found...');
    }
  };

  const handleLookup = async (e: React.FormEvent) => {
    e.preventDefault();
    setErrorMessage('');
    setSuccessMessage('');
    setUser(null);

    if (!userId.trim()) {
      setErrorMessage('User ID can NOT be empty...');
      return;
    }

    try {
      setLoading(true);
      const data = await userService.getUserByUserId(userId);
      setUser(data);
      setSuccessMessage('Press Delete button to delete this user ...');
    } catch (err) {
      setErrorMessage('User ID NOT found...');
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!user) return;
    
    if (!confirm(`Are you sure you want to delete user ${user.userId}?`)) return;
    
    try {
      setLoading(true);
      await userService.deleteUser(user.id);
      setSuccessMessage(`User ${user.userId} deleted successfully!`);
      setUser(null);
      setUserId('');
      
      setTimeout(() => {
        router.push('/users');
      }, 2000);
    } catch (err) {
      setErrorMessage('Unable to delete user...');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleClear = () => {
    setUser(null);
    setUserId('');
    setErrorMessage('');
    setSuccessMessage('');
  };

  return (
    <div className="min-h-screen bg-gray-50 flex flex-col">
      <div className="bg-blue-900 text-white p-4">
        <div className="max-w-7xl mx-auto flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">AWS Mainframe Modernization</h1>
            <h2 className="text-lg">CardDemo - Delete User</h2>
          </div>
          <div className="text-right">
            <div className="text-sm">Date: {currentDate}</div>
            <div className="text-sm">Time: {currentTime}</div>
          </div>
        </div>
      </div>

      <div className="flex-1 p-6">
        <div className="max-w-2xl mx-auto">
          <div className="bg-white shadow-lg rounded-lg p-8">
            <div className="mb-6">
              <div className="text-sm text-gray-600 mb-4">
                <span>Transaction: COUSR</span>
                <span className="ml-4">Program: COUSR03C</span>
              </div>
              <h3 className="text-xl font-bold text-gray-800">Delete User</h3>
            </div>

            {errorMessage && (
              <div className="mb-4 p-3 bg-red-50 border border-red-200 rounded text-red-700 text-sm">
                {errorMessage}
              </div>
            )}

            {successMessage && (
              <div className="mb-4 p-3 bg-green-50 border border-green-200 rounded text-green-700 text-sm">
                {successMessage}
              </div>
            )}

            <form onSubmit={handleLookup} className="space-y-4">
              <Input
                label="User ID"
                value={userId}
                onChange={(e) => setUserId(e.target.value.toUpperCase())}
                maxLength={8}
                required
                autoFocus
                disabled={!!user}
              />

              {user && (
                <div className="space-y-4 p-4 bg-gray-50 rounded-lg">
                  <div>
                    <label className="block text-sm font-semibold text-gray-700 mb-1">First Name</label>
                    <p className="text-gray-900">{user.firstName}</p>
                  </div>
                  
                  <div>
                    <label className="block text-sm font-semibold text-gray-700 mb-1">Last Name</label>
                    <p className="text-gray-900">{user.lastName}</p>
                  </div>
                  
                  <div>
                    <label className="block text-sm font-semibold text-gray-700 mb-1">User Type</label>
                    <p className="text-gray-900">
                      <span className={`px-3 py-1 rounded text-sm font-semibold ${
                        user.userType === 'A' ? 'bg-purple-100 text-purple-800' : 'bg-blue-100 text-blue-800'
                      }`}>
                        {user.userTypeDisplay}
                      </span>
                    </p>
                  </div>
                </div>
              )}
              
              <div className="flex gap-2 pt-4">
                {!user ? (
                  <>
                    <Button type="submit" disabled={loading}>
                      {loading ? 'Looking up...' : 'Lookup User'}
                    </Button>
                    <Button
                      type="button"
                      variant="secondary"
                      onClick={() => router.push('/users')}
                    >
                      Back to List
                    </Button>
                  </>
                ) : (
                  <>
                    <Button 
                      type="button" 
                      variant="danger" 
                      onClick={handleDelete}
                      disabled={loading}
                    >
                      {loading ? 'Deleting...' : 'Delete User'}
                    </Button>
                    <Button
                      type="button"
                      variant="secondary"
                      onClick={handleClear}
                    >
                      Clear
                    </Button>
                    <Button
                      type="button"
                      variant="secondary"
                      onClick={() => router.push('/users')}
                    >
                      Back to List
                    </Button>
                  </>
                )}
              </div>
            </form>
          </div>
        </div>
      </div>
    </div>
  );
}
